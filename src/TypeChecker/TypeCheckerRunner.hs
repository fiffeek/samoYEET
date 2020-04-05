module TypeChecker.TypeCheckerRunner where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative
import           TypeChecker.TypeChecker
import           TypeChecker.TypeError
import           TypeChecker.Environment
import           Common.CommandLineHelpers
import           System.IO
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

execTypeCheckerMonad :: CommandLineArguments -> [Stmt] -> IO () -> IO ()
execTypeCheckerMonad cla stmts =
  runTypeCheckerMonad initialEnvironment (typeCheckStmtsM stmts)

errorsHandler :: TypeError -> IO ()
errorsHandler error = do
  hPutStrLn stderr . addPrefix . go $ error
  exitFailure
 where
  addPrefix = (++) "Type error: "
  go (TypeMismatch actual expected) =
    "expected = [" ++ (show expected) ++ "] actual = [" ++ (show actual) ++ "]"
  go (NotInitialized idn) = "variable = [" ++ (show idn) ++ "] not initialized"
  go WrongNumberOfArguments         = "wrong number of arguments"
  go FunctionBodyDoesNotReturnValue = "f"
  go _                              = "Unknown error"

runTypeCheckerMonad :: Env -> TypeCheckerMonad Env -> IO () -> IO ()
runTypeCheckerMonad env m cont = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right _   -> cont
 where
  r = runReaderT m env
  e = runExceptT r
