module Interpreter.RuntimeError where
import           Samoyeet.Abs


data RuntimeError = UnknownError
  | DivisionByZero
  | VariableNotInitialized Ident Stmt
  | ValueNotReturned deriving Show
