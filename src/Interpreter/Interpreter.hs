module Interpreter.Interpreter
  ( execStatementsM
  )
where
import           Interpreter.Environment
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Interpreter.Types
import qualified Data.Map                      as M
import           Interpreter.RuntimeError

evaluateExprM :: Expr -> InterpretMonad VType
evaluateExprM (ELitInt val           ) = return (VInt val)
evaluateExprM (ELitTrue              ) = return (VBool True)
evaluateExprM (ELitFalse             ) = return (VBool False)
evaluateExprM (EString val           ) = return (VStr val)
evaluateExprM (ELambda args ret block) = do
  env <- ask
  return $ VFun args block env ret
evaluateExprM (Neg expr) = do
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
evaluateExprM (EVar var       ) = getById var
evaluateExprM (EApp name exprs) = do
  pEnv    <- asks pEnv
  currEnv <- ask
  (FunctionDefinition _ _ args b@(Block stmts) envF) <- getOrError
    (M.lookup name pEnv)
    VariableNotInitialized
  let argExpr = args `zip` exprs
  envAfterVDecl <- local (const envF) $ dispatcher argExpr currEnv
  envAfterFDecl <- local (const envAfterVDecl) $ execStatementsM stmts
  when (vtype envAfterFDecl == VNone) $ throwError ValueNotReturned
  return (vtype envAfterFDecl)
 where
  dispatcher :: [(Arg, Expr)] -> Env -> InterpretMonad Env
  dispatcher []                          _       = ask
  dispatcher (((Arg _ name), expr) : xs) evalEnv = do
    val    <- local (const evalEnv) $ evaluateExprM expr
    curEnv <- ask
    case val of
      (VFun args body env ret) -> do
        let envF = declareFunWithEnv ret name args body env curEnv
        local (const envF) $ dispatcher xs evalEnv
      _ -> do
        env <- putValInit name val
        local (const env) $ dispatcher xs evalEnv
  dispatcher (((RefArg _ name), (EVar calledWith)) : xs) evalEnv = do
    loc <- getOrError (M.lookup calledWith $ vEnv evalEnv)
                      VariableNotInitialized
    local (putVal name loc) $ dispatcher xs evalEnv

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
execStatementM (Decl _           []                  ) = ask
execStatementM (Decl t@(Fun _ _) ((NoInit name) : xs)) = do
  env <- shadowName name
  local (const env) $ execStatementM (Decl t xs)
execStatementM (Decl t ((NoInit name) : xs)) = do
  env <- putValNoInit name
  local (const env) $ execStatementM (Decl t xs)
execStatementM (Decl t@(Fun _ _) ((Init name expr) : xs)) = do
  (VFun args body env ret) <- evaluateExprM expr
  curEnv                   <- ask
  let modifiedEnv = declareFunWithEnv ret name args body env curEnv
  local (const modifiedEnv) $ execStatementM (Decl t xs)
execStatementM (Decl t ((Init name expr) : xs)) = do
  val <- evaluateExprM expr
  env <- putValInit name val
  local (const env) $ execStatementM (Decl t xs)
execStatementM (Print expr) = do
  val <- evaluateExprM expr
  liftIO . putStrLn . show $ val
  ask
execStatementM (Ass name expr) = do
  val    <- evaluateExprM expr
  curEnv <- ask
  case val of
    (VFun args body env ret) ->
      return $ declareFunWithEnv ret name args body env curEnv
    _ -> do
      env <- asks vEnv
      loc <- getOrError (M.lookup name env) VariableNotInitialized
      CMS.modify (putValInAddr val loc)
      return $ curEnv
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
