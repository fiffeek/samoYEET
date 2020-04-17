module TypeChecker.Runner
  ( execTypeCheckerMonad
  )
where
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
import           TypeChecker.ReturnChecker

execTypeCheckerMonad :: CommandLineArguments -> [Stmt] -> IO () -> IO ()
execTypeCheckerMonad cla stmts cont = do
  runTypeCheckerMonad initialEnvironment (typeCheckStmtsM stmts)
    $ runTypeCheckerMonad initialEnvironment (returnCheckStmtsM stmts) cont

errorsHandler :: TypeError -> IO ()
errorsHandler error = do
  hPutStrLn stderr . addPrefix . go $ error
  exitFailure
 where
  addPrefix = (++) "Type error: "
  go (TypeMismatch actual expected) =
    "expected any of " ++ (show expected) ++ ", actual type " ++ (show actual)
  go (NotInitialized idn)           = show idn ++ " not initialized"
  go WrongNumberOfArguments = "Wrong number of arguments passed to function"
  go (BadReference) = "Bad reference, referenced lvalue instead of rvalue"
  go FunctionBodyDoesNotReturnValue = "Missing return in function body"
  go (OutsideOfLoop _         )     = "Statement outside of loop"
  go (FunctionNotReferenceable)     = "Functions are not referenceable"
  go (ConflictingDeclarations )     = "Redeclaration, conflicting types"
  go (Redeclaration           )     = "Redeclaration of variable"
  go (ReturnMissing           )     = "Missing return in function body"
  go _                              = "Unknown error"

runTypeCheckerMonad :: Env -> TypeCheckerMonad a -> IO () -> IO ()
runTypeCheckerMonad env m cont = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right _   -> cont
 where
  r = runReaderT m env
  e = runExceptT r
