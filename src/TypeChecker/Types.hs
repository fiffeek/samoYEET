module TypeChecker.Types where
import           Samoyeet.Abs
import           Common.Types

type TCContext = Maybe (Int, Int)
type TCExpr = Expr TCContext
type TCArg = Arg TCContext
type TCMaybeRefType = MaybeRefType TCContext
type TCStmt = Stmt TCContext
type TCSType = SType TCContext

dropContext :: TCSType -> TCSType
dropContext (Int  _) = Int Nothing
dropContext (Str  _) = Str Nothing
dropContext (Bool _) = Bool Nothing
dropContext (Void _) = Void Nothing
dropContext (Fun _ retType args) =
  Fun Nothing (dropContext retType) (fmap dropContextArg args)

dropContextArg :: TCMaybeRefType -> TCMaybeRefType
dropContextArg (NoRef _ actualType) = NoRef Nothing (dropContext actualType)
dropContextArg (JustRef _ actualType) =
  JustRef Nothing (dropContext actualType)
