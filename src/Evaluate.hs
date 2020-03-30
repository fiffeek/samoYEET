module Evaluate where
import           Environments
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Utils
import           ValueTypes

evaluateExprM :: Expr -> InterpretMonad VType
evaluateExprM (ELitInt val) = return (VInt val)

execStatementM :: Stmt -> InterpretMonad VType
execStatementM (Ret expr) = evaluateExprM expr

execStatementsM :: [Stmt] -> InterpretMonad VType
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = do
  execStatementM x
  execStatementsM xs

execInterpretMonad :: [Stmt] -> IO ()
execInterpretMonad =
  runInterpretMonad initialEnvironment initialState . execStatementsM

errorsHandler :: RuntimeError -> IO ()
errorsHandler _ = putStrLn "Error"

runInterpretMonad :: Env -> Store -> InterpretMonad VType -> IO ()
runInterpretMonad env state m = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right val -> do
      putStrLn $ returnProgram val
      pure ()
 where
  r = runReaderT m env
  s = evalStateT r state
  e = runExceptT s

