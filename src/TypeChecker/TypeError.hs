module TypeChecker.TypeError where
import           Samoyeet.Abs

data TypeError = NotAFunction
  | TypeMismatch SType [SType]
  | NotInitialized Ident
  | FunctionBodyDoesNotReturnValue
  | OutsideOfLoop Stmt
  | WrongNumberOfArguments
  | FunctionNotReferenceable
  | ConflictingDeclarations
  | Redeclaration
  | BadReference
  | ReturnMissing
  | UnknownError deriving Show
