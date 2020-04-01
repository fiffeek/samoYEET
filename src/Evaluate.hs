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
import           Expressions
import           Statements

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

