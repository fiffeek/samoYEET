module Interpreter.RuntimeError where


data RuntimeError = UnknownError
  | DivisionByZero
  | VariableNotInitialized
  | ValueNotReturned deriving Show
