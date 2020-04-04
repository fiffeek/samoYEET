module Interpreter.RuntimeError where


data RuntimeError = UnknownError
  | VariableNotInitialized
  | VariableMissingInStore
  | DivisionByZero
  | IncompatibleTypes
  | FunctionNotInitialized
  | WrongNumberOfArguments
  | VariableNotInScope
  | VariableNotReferencable deriving Show
