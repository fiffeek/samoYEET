module TypeChecker.Types where
import           Samoyeet.Abs

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

getContext :: TCStmt -> Maybe (Int, Int)
getContext (Empty ctx         ) = ctx
getContext (Decl ctx _ _      ) = ctx
getContext (Ass  ctx _ _      ) = ctx
getContext (Incr ctx _        ) = ctx
getContext (Decl ctx _ _      ) = ctx
getContext (Ret ctx _         ) = ctx
getContext (VRet ctx          ) = ctx
getContext (Cond ctx _ _      ) = ctx
getContext (CondElse ctx _ _ _) = ctx
getContext (While ctx _ _     ) = ctx
getContext (SBreak    ctx     ) = ctx
getContext (SContinue ctx     ) = ctx
getContext (SExp  ctx _       ) = ctx
getContext (Print ctx _       ) = ctx
getContext (SFnDef ctx _ _ _ _) = ctx

typeWithContext (Int  ctx   ) = (ctx, "integer")
typeWithContext (Str  ctx   ) = (ctx, "string")
typeWithContext (Bool ctx   ) = (ctx, "boolean")
typeWithContext (Void ctx   ) = (ctx, "void")
typeWithContext (Fun ctx _ _) = (ctx, "function")
