module Interpreter.RuntimeError where
import           Samoyeet.Abs
import           Common.Types

type IStmt = Stmt GenericContext

data RuntimeError = UnknownError
  | DivisionByZero IStmt
  | VariableNotInitialized Ident IStmt
  | ValueNotReturned deriving Show
