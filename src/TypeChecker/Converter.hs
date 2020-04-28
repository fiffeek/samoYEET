module TypeChecker.Converter where
import           Samoyeet.Abs
import           TypeChecker.Types

argToSType :: Arg a -> SType a
argToSType (Arg    _ t _) = t
argToSType (RefArg _ t _) = t

argToRefType :: Arg a -> MaybeRefType a
argToRefType (Arg    l t _) = NoRef l t
argToRefType (RefArg l t _) = JustRef l t

argToNoInit :: Arg a -> Stmt a
argToNoInit (Arg    l t n) = Decl l t [NoInit l n]
argToNoInit (RefArg l t n) = Decl l t [NoInit l n]

maybeRefToSType :: MaybeRefType a -> SType a
maybeRefToSType (NoRef   _ t) = t
maybeRefToSType (JustRef _ t) = t
