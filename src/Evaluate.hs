module Evaluate where
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
evaluateExprM (EApp ident [exs]) = undefined
evaluateExprM (EVar var        ) = do
  env   <- asks vEnv
  loc   <- getOrError (M.lookup var env) VariableNotInitialized
  state <- getStorage CMS.get
  getOrError (M.lookup loc state) VariableMissingInStore

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
execStatementM (Decl t []                  ) = ask
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
execStatementM (Incr name                ) = addToVar name 1
execStatementM (Decr name                ) = addToVar name (-1)
execStatementM (Cond expr stmt) = execStatementM (CondElse expr stmt Empty)
execStatementM (CondElse expr stmt1 stmt2) = do
  val <- evaluateExprM expr
  liftM2
    flushVariables
    ask
    (if val == (VBool True) then execStatementM stmt1 else execStatementM stmt2)


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

execInterpretMonad :: [Stmt] -> IO ()
execInterpretMonad =
  runInterpretMonad initialEnvironment initialState . execStatementsM

errorsHandler :: RuntimeError -> IO ()
errorsHandler error = putStrLn . addPrefix . go $ error
 where
  addPrefix = (++) "Error: "
  go VariableNotInitialized = "Variable was not initialized"
  go VariableMissingInStore = "Variable was not initialized"
  go DivisionByZero         = "Division by zero"

runInterpretMonad :: Env -> Store -> InterpretMonad Env -> IO ()
runInterpretMonad env state m = do
  ans <- e
  case ans of
    Left  err               -> errorsHandler $ err
    Right env@(Env _ _ val) -> do
      putStrLn $ show env
      putStrLn $ returnProgram val
      pure ()
 where
  r = runReaderT m env
  s = evalStateT r state
  e = runExceptT s

