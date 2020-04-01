module RuntimeError where


data RuntimeError = UnknownError
  | VariableNotInitialized
  | VariableMissingInStore
  | DivisionByZero
  | IncompatibleTypes
  | FunctionNotInitialized
  | WrongNumberOfArguments deriving Show
