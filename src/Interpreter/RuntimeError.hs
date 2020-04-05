module Interpreter.RuntimeError where


data RuntimeError = UnknownError
  | DivisionByZero
  | ValueNotReturned deriving Show
