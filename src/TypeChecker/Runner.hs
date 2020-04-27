module TypeChecker.Runner
  ( execTypeCheckerMonad
  )
where
import           Samoyeet.Abs
import           Samoyeet.Print
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
import           Common.Utils

execTypeCheckerMonad :: CommandLineArguments -> [Stmt] -> IO () -> IO ()
execTypeCheckerMonad cla stmts =
  runTypeCheckerMonad initialEnvironment (typeCheckStmtsM stmts)
    . runTypeCheckerMonad initialEnvironment (returnCheckStmtsM stmts)

errorsHandler :: TypeError -> IO ()
errorsHandler error = do
  hPutStrLn stderr . addPrefix . go $ error
  exitFailure
 where
  addPrefix    = (++) "Type error: "
  stripEndLine = filter (/= '\n')
  addContext mess stmt =
    concat [mess, " in\n `", stripEndLine . printTree $ stmt, "` \n"]
  go (TypeMismatch actual expected context) = showTextSeq
    [ addContext "type mismatch" context
    , "expected any of"
    , show expected
    , "actual type"
    , show actual
    ]
  go (NotInitialized idn context) =
    showTextSeq [addContext ((show idn) ++ " not initialized") context]
  go (WrongNumberOfArguments name) =
    showTextSeq ["Wrong number of arguments passed to a", show name, "function"]
  go (BadReference ref) = showTextSeq
    [addContext "bad reference" ref, "referenced lvalue instead of rvalue"]
  go (FunctionBodyDoesNotReturnValue name) =
    showTextSeq ["Missing return in", show name, "function body"]
  go (OutsideOfLoop stmt      ) = addContext "Statement outside of a loop" stmt
  go (FunctionNotReferenceable) = "Functions are not referenceable"
  go (ConflictingDeclarations idn stmt) =
    addContext ("Redeclaration, conflicting types for " ++ (show idn)) stmt
  go (Redeclaration idn stmt) = go (ConflictingDeclarations idn stmt)
  go (ReturnMissing name    ) = go (FunctionBodyDoesNotReturnValue name)
  go _                        = "Unknown error"

runTypeCheckerMonad :: Env -> TypeCheckerMonad a -> IO () -> IO ()
runTypeCheckerMonad env m cont = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right _   -> cont
 where
  r = runReaderT m env
  e = runExceptT r
