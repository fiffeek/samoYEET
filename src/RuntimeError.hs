module RuntimeError where


data RuntimeError = UnknownError | VariableNotInitialized | VariableMissingInStore | DivisionByZero | IncompatibleTypes deriving Show
