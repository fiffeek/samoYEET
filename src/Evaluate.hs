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

execStatementM :: Stmt -> InterpretMonad (Env, VType)
execStatementM (Ret expr) = do
  env <- ask
  val <- evaluateExprM expr
  return (env, val)
execStatementM (VRet) = do
  env <- ask
  return (env, VNone)
execStatementM (BStmt (Block stmts)) = do
  env      <- ask
  (_, val) <- execStatementsM stmts
  return (env, val)
execStatementM (Empty) = do
  env <- ask
  return (env, VNone)
execStatementM (Decl t []) = do
  env <- ask
  return (env, VNone)
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
  env <- ask
  val <- evaluateExprM expr
  liftIO . putStrLn . show $ val
  return (env, VNone)

execStatementsM :: [Stmt] -> InterpretMonad (Env, VType)
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = do
  (env, val) <- execStatementM x
  statementValueDispatcher val env (execStatementsM xs)
 where
  statementValueDispatcher
    :: VType
    -> Env
    -> InterpretMonad (Env, VType)
    -> InterpretMonad (Env, VType)
  statementValueDispatcher (VNone) env stmts = local (const env) stmts
  statementValueDispatcher other   env _     = return (env, other)

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

runInterpretMonad :: Env -> Store -> InterpretMonad (Env, VType) -> IO ()
runInterpretMonad env state m = do
  ans <- e
  case ans of
    Left  err        -> errorsHandler $ err
    Right (env, val) -> do
      putStrLn $ show env
      putStrLn $ returnProgram val
      pure ()
 where
  r = runReaderT m env
  s = evalStateT r state
  e = runExceptT s

