module TypeChecker.TypeError where
import           Samoyeet.Abs

data TypeError = NotAFunction
  | TypeMismatch SType [SType] Stmt
  | NotInitialized Ident Stmt
  | FunctionBodyDoesNotReturnValue Ident
  | OutsideOfLoop Stmt
  | WrongNumberOfArguments Ident
  | FunctionNotReferenceable
  | ConflictingDeclarations Ident Stmt
  | Redeclaration Ident Stmt
  | BadReference Stmt
  | ReturnMissing Ident
  | UnknownError deriving Show
