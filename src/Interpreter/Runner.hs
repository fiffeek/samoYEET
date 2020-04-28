module Interpreter.Runner
  ( execInterpretMonad
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
import           Interpreter.Interpreter
import           System.IO
import           System.Exit
import           Common.Utils
import           Samoyeet.Print

execInterpretMonad :: [IStmt] -> IO ()
execInterpretMonad =
  runInterpretMonad initialEnvironment initialState . execStatementsM

errorsHandler :: RuntimeError -> IO ()
errorsHandler error = do
  hPutStrLn stderr . addPrefix . go $ error
  exitFailure
 where
  addPrefix = (++) "Error: "
  go DivisionByZero   = "Division by zero"
  go ValueNotReturned = "Function did not return a value"
  go (VariableNotInitialized name ctx) =
    showTextSeq ["Variable", show name, "not initialized in", printTree ctx]
  go _ = "Unknown error"

runInterpretMonad :: Env -> Store -> InterpretMonad Env -> IO ()
runInterpretMonad env state m = do
  ans <- e
  case ans of
    Left  err                  -> errorsHandler $ err
    Right (Env _ _ (VInt 0) _) -> exitSuccess
    Right (Env _ _ (VInt val) _) ->
      exitWith (ExitFailure . fromIntegral $ val) >> pure ()
    Right (Env _ _ _ _) -> exitSuccess
 where
  r = runReaderT m env
  s = evalStateT r state
  e = runExceptT s

