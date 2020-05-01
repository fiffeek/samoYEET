module TypeChecker.TypeError where
import           Samoyeet.Abs
import           TypeChecker.Types

data TypeMismatchContext = CannotCompare
  | ReturnType TCSType
  | LHSRHS
  | FunArgType
  | BinaryInt
  | BinaryBool
  | OpInt
  | OpBool
  | ComparableTypes
  | OpPlus
  | PrintT
  | Assignment TCSType
  | DeclVaries TCSType
  | ConditionBool deriving Show

data TypeError = NotAFunction
  | TypeMismatch TypeMismatchContext TCSType [TCSType] TCStmt
  | NotInitialized Ident TCStmt
  | FunctionBodyDoesNotReturnValue Ident
  | OutsideOfLoop TCStmt
  | WrongNumberOfArguments Ident
  | FunctionNotReferenceable
  | ConflictingDeclarations Ident TCStmt
  | Redeclaration Ident TCStmt
  | BadReference TCStmt
  | ReturnMissing Ident
  | UnknownError deriving Show
