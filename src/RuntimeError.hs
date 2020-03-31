module RuntimeError where


data RuntimeError = UnknownError | VariableNotInitialized | VariableMissingInStore | DivisionByZero deriving Show
