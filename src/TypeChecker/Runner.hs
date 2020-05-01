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
import           TypeChecker.Types
import           Data.List

execTypeCheckerMonad :: CommandLineArguments -> [TCStmt] -> IO () -> IO ()
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
    let maybeCtx = getContext stmt
    in  case maybeCtx of
          (Just (column, line)) -> concat
            [ "\n"
            , "(line "
            , show line
            , ", column "
            , show column
            , ") "
            , mess
            , " in\n```"
            , stripEndLine . printTree $ stmt
            , "```\n"
            ]
          (Nothing) -> concat
            ["\n", mess, " in\n```", stripEndLine . printTree $ stmt, "```\n"]
  typeWithLine tctype =
    let (maybeCtx, typeString) = typeWithContext tctype
    in  case maybeCtx of
          (Just (line, column)) -> concat
            ["(line ", show line, ", column ", show column, ") ", typeString]
          (Nothing) -> concat [typeString]
  mismatchGeneric context = addContext "type mismatch" context
  showTypes actual expected = concat
    [ "actual type "
    , typeWithLine actual
    , "\nexpected type(s) ["
    , concat . intersperse ", " . map typeWithLine $ expected
    , "]"
    ]
  showMismatchGeneric msg actual expected context =
    concat [mismatchGeneric context, msg, "\n", showTypes actual expected]
  go (TypeMismatch (ReturnType oldType) actual expected context) = concat
    [ mismatchGeneric context
    , "return type does not match previously declared\n"
    , typeWithLine oldType
    ]
  go (TypeMismatch FunArgType actual expected context) = showMismatchGeneric
    "argument passed to a function is of a wrong type"
    actual
    expected
    context
  go (TypeMismatch BinaryInt actual expected context) =
    showMismatchGeneric "binary integer operator" actual expected context
  go (TypeMismatch BinaryBool actual expected context) =
    showMismatchGeneric "binary boolean operator" actual expected context
  go (TypeMismatch OpInt actual expected context) = showMismatchGeneric
    "integer operator expects two integers"
    actual
    expected
    context
  go (TypeMismatch OpBool actual expected context) = showMismatchGeneric
    "boolean operator expects two booleans"
    actual
    expected
    context
  go (TypeMismatch ComparableTypes actual expected context) =
    showMismatchGeneric "types are not comparable" actual expected context
  go (TypeMismatch OpPlus actual expected context) = showMismatchGeneric
    "operator plus expects two integers or two strings"
    actual
    expected
    context
  go (TypeMismatch LHSRHS actual expected context) = showMismatchGeneric
    "left hand side is of different type than right hand side"
    actual
    expected
    context
  go (TypeMismatch PrintT actual expected context) = showMismatchGeneric
    "currently supported print types are: [integer, string, boolean]"
    actual
    expected
    context
  go (TypeMismatch (Assignment tctype) _ expected context) =
    showMismatchGeneric "tried to assign different types"
                        tctype
                        expected
                        context
  go (TypeMismatch (DeclVaries tctype) _ expected context) =
    showMismatchGeneric
      "declaration type mismatch, declared type is different to the asignee"
      tctype
      expected
      context
  go (TypeMismatch ConditionBool actual expected context) = showMismatchGeneric
    "condition must evaluate to a boolean value"
    actual
    expected
    context
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
