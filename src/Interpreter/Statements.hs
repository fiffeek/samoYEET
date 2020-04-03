module Interpreter.Statements where
import           Interpreter.Environments
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Interpreter.Utils
import           Interpreter.ValueTypes
import qualified Data.Map                      as M
import           Interpreter.RuntimeError

getOrError :: Maybe a -> RuntimeError -> InterpretMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

throwOnPredicate predicate error =
  if predicate then throwError error else return ()

evaluateExprM :: Expr -> InterpretMonad VType
evaluateExprM (ELitInt val ) = return (VInt val)
evaluateExprM (ELitTrue    ) = return (VBool True)
evaluateExprM (ELitFalse   ) = return (VBool False)
evaluateExprM (EString val ) = return (VStr val)
evaluateExprM (Neg     expr) = do
  fmap myNeg (evaluateExprM expr)
evaluateExprM (Not expr) = do
  fmap myNot (evaluateExprM expr)
evaluateExprM (EMul left op right) =
  myMul op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAdd left op right) =
  myAdd op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (ERel left op right) =
  myRel op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAnd left right) =
  liftM2 myAnd (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EOr left right) =
  liftM2 myOr (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EVar var) = do
  env   <- asks vEnv
  loc   <- getOrError (M.lookup var env) VariableNotInitialized
  state <- getStorage CMS.get
  getOrError (M.lookup loc state) VariableMissingInStore
evaluateExprM (EApp name exprs) = do
  pEnv    <- asks pEnv
  currEnv <- ask
  (FunctionDefinition retType name args b@(Block stmts) envF) <- getOrError
    (M.lookup name pEnv)
    FunctionNotInitialized
  throwOnPredicate (length args /= length exprs) WrongNumberOfArguments
  let argExpr = args `zip` exprs
  envAfterVDecl <- local (const envF) $ dispatcher argExpr currEnv
  let envFunc = putFunInEnv retType name args b envAfterVDecl
  envAfterFDecl <- local (const envFunc) $ execStatementsM stmts
  return (vtype envAfterFDecl)
 where
  dispatcher :: [(Arg, Expr)] -> Env -> InterpretMonad Env
  dispatcher []                          _       = ask
  dispatcher (((Arg _ name), expr) : xs) evalEnv = do
    val <- local (const evalEnv) $ evaluateExprM expr
    env <- putVal name val
    vTypeExprDispatcher val
                        (local (const env) $ dispatcher xs evalEnv)
                        (throwError IncompatibleTypes)
  dispatcher (((RefArg _ name), (EVar calledWith)) : xs) evalEnv = do
    loc <- getOrError (M.lookup calledWith $ vEnv evalEnv)
                      VariableNotReferencable
    local (putValInEnv name loc) $ dispatcher xs evalEnv
  dispatcher _ _ = throwError VariableNotReferencable

execStatementM :: Stmt -> InterpretMonad Env
execStatementM (Ret expr) = do
  liftM2 changeRetType (evaluateExprM expr) ask
execStatementM (VRet) = do
  env <- ask
  return $ changeRetType VVoid env
execStatementM (BStmt (Block stmts)) = do
  liftM2 flushVariables ask (execStatementsM stmts)
execStatementM (Empty ) = ask
execStatementM (SBreak) = do
  env <- ask
  return $ changeRetType VBreak env
execStatementM (SContinue) = do
  env <- ask
  return $ changeRetType VContinue env
execStatementM (Decl _ []                  ) = ask
execStatementM (Decl t ((NoInit name) : xs)) = do
  env <- putValNoInit name
  local (const env) $ execStatementM (Decl t xs)
execStatementM (Decl t ((Init name expr) : xs)) = do
  val <- evaluateExprM expr
  env <- putVal name val
  local (const env) $ execStatementM (Decl t xs)
execStatementM (Print expr) = do
  val <- evaluateExprM expr
  liftIO . putStrLn . show $ val
  ask
execStatementM (Ass name expr) = do
  val <- evaluateExprM expr
  env <- asks vEnv
  loc <- getOrError (M.lookup name env) VariableNotInitialized
  CMS.modify (putValInAddr val loc)
  ask
execStatementM (Incr name) = addToVar name 1
execStatementM (Decr name) = addToVar name (-1)
execStatementM (SExp expr) = do
  evaluateExprM expr
  ask
execStatementM (Cond expr stmt) = execStatementM (CondElse expr stmt Empty)
execStatementM (CondElse expr stmt1 stmt2) = do
  val <- evaluateExprM expr
  liftM2
    flushVariables
    ask
    (if val == (VBool True) then execStatementM stmt1 else execStatementM stmt2)
execStatementM loop@(While expr stmt) = do
  val <- evaluateExprM expr
  if val == (VBool True)
    then do
      env <- execStatementM stmt
      matchReturnType env (execStatementM loop)
    else ask
 where
  matchReturnType :: Env -> InterpretMonad Env -> InterpretMonad Env
  matchReturnType env@(Env _ _ VNone    ) stmt = local (const env) stmt
  matchReturnType (    Env _ _ VBreak   ) _    = local (changeRetType VNone) ask
  matchReturnType (Env _ _ VContinue) stmt = local (changeRetType VNone) stmt
  matchReturnType (    Env _ _ t        ) _    = local (changeRetType t) ask
execStatementM (SFnDef retType name args block) = do
  env <- ask
  return (putFunInEnv retType name args block env)


addToVar :: Ident -> Integer -> InterpretMonad Env
addToVar name toAdd = do
  env   <- asks vEnv
  loc   <- getOrError (M.lookup name env) VariableNotInitialized
  state <- CMS.get
  val   <- getOrError (M.lookup loc (storage state)) VariableNotInitialized
  let inc = myPlus val (VInt toAdd)
  CMS.modify (putValInAddr inc loc)
  ask

execStatementsM :: [Stmt] -> InterpretMonad Env
execStatementsM []       = ask
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = do
  env <- execStatementM x
  statementValueDispatcher env (execStatementsM xs)

statementValueDispatcher :: Env -> InterpretMonad Env -> InterpretMonad Env
statementValueDispatcher env@(Env _ _ VNone) stmts = local (const env) stmts
statementValueDispatcher env                 _     = return env

vTypeExprDispatcher
  :: VType -> InterpretMonad Env -> InterpretMonad Env -> InterpretMonad Env
vTypeExprDispatcher (VInt  _) m1 _  = m1
vTypeExprDispatcher (VBool _) m1 _  = m1
vTypeExprDispatcher (VStr  _) m1 _  = m1
vTypeExprDispatcher _         _  m2 = m2
