module Interpreter.RuntimeError where
import           Samoyeet.Abs

type IStmt = Stmt (Maybe (Int, Int))

data RuntimeError = UnknownError
  | DivisionByZero
  | VariableNotInitialized Ident IStmt
  | ValueNotReturned deriving Show
