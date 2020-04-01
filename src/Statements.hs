module Statements where
import           Environments
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Utils
import           ValueTypes
import qualified Data.Map                      as M
import           RuntimeError

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
  env <- asks pEnv
  (FunctionDefinition retType name args (Block stmts) env) <- getOrError
    (M.lookup name env)
    FunctionNotInitialized
  throwOnPredicate (length args /= length exprs) WrongNumberOfArguments
  let zipped  = zip args exprs
  let mapped = map (\((Arg t name), exp) -> Decl t [Init name exp]) zipped
  let app     = mapped ++ stmts
  let blocked = BStmt . Block $ app
  env <- execStatementM blocked
  return (vtype env)


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
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  local (putValInEnv name loc) $ execStatementM (Decl t xs)
execStatementM (Decl t ((Init name expr) : xs)) = do
  val   <- evaluateExprM expr
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  CMS.modify (putValInStore val splitState)
  local (putValInEnv name loc) $ execStatementM (Decl t xs)
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
  matchReturnType env@(Env _ _ VBreak) _ = local (changeRetType VNone) ask
  matchReturnType env@(Env _ _ VNone) stmt = local (const env) stmt
  matchReturnType (Env _ _ VContinue) stmt = local (changeRetType VNone) stmt
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
 where
  statementValueDispatcher :: Env -> InterpretMonad Env -> InterpretMonad Env
  statementValueDispatcher env@(Env _ _ VNone) stmts = local (const env) stmts
  statementValueDispatcher env                 _     = return env
