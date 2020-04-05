module Interpreter.Evaluate where
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
import           Interpreter.Statements
import           System.IO
import           System.Exit

execInterpretMonad :: [Stmt] -> IO ()
execInterpretMonad =
  runInterpretMonad initialEnvironment initialState . execStatementsM

errorsHandler :: RuntimeError -> IO ()
errorsHandler error = do
  hPutStrLn stderr . addPrefix . go $ error
  exitFailure
 where
  addPrefix = (++) "Error: "
  go VariableNotInitialized = "Variable was not initialized"
  go VariableMissingInStore = "Variable was not initialized"
  go DivisionByZero         = "Division by zero"
  go WrongNumberOfArguments = "Wrong number of arguments passed to function"
  go IncompatibleTypes      = "Incompatible types"
  go FunctionNotInitialized = "Function not in scope"
  go _                      = "Unknown error"

runInterpretMonad :: Env -> Store -> InterpretMonad Env -> IO ()
runInterpretMonad env state m = do
  ans <- e
  case ans of
    Left  err                    -> errorsHandler $ err
    Right env@(Env _ _ (VInt 0)) -> exitSuccess
    Right env@(Env _ _ (VInt val)) ->
      exitWith (ExitFailure . fromIntegral $ val) >> pure ()
    Right env@(Env _ _ _) -> exitSuccess
 where
  r = runReaderT m env
  s = evalStateT r state
  e = runExceptT s

