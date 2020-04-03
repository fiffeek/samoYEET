module TypeChecker.Converter where
import           Samoyeet.Abs

argToSType :: Arg -> SType
argToSType (Arg    t _) = t
argToSType (RefArg t _) = t

argToRefType :: Arg -> MaybeRefType
argToRefType (Arg    t _) = NoRef t
argToRefType (RefArg t _) = JustRef t

argToNoInit :: Arg -> Stmt
argToNoInit (Arg    t n) = Decl t [NoInit n]
argToNoInit (RefArg t n) = Decl t [NoInit n]
