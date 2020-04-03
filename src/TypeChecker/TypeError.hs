module TypeChecker.TypeError where
import           Samoyeet.Abs

data TypeError = NotAFunction
  | TypeMismatch SType [SType]
  | NotInitialized Ident
  | FunctionBodyDoesNotReturnValue
  | OutsideOfLoop Stmt
  | WrongNumberOfArguments
  | UnknownError deriving Show
