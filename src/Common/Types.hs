module Common.Types where
import           Samoyeet.Abs

type GenericContext = Maybe (Int, Int)
type GenericStmt = Stmt GenericContext
type GenericExpr = Expr GenericContext

getContext :: GenericStmt -> GenericContext
getContext (Empty ctx         ) = ctx
getContext (Decl ctx _ _      ) = ctx
getContext (Ass  ctx _ _      ) = ctx
getContext (Incr ctx _        ) = ctx
getContext (Decr ctx _        ) = ctx
getContext (Ret  ctx _        ) = ctx
getContext (VRet ctx          ) = ctx
getContext (Cond ctx _ _      ) = ctx
getContext (CondElse ctx _ _ _) = ctx
getContext (While ctx _ _     ) = ctx
getContext (SBreak    ctx     ) = ctx
getContext (SContinue ctx     ) = ctx
getContext (SExp  ctx _       ) = ctx
getContext (Print ctx _       ) = ctx
getContext (SFnDef ctx _ _ _ _) = ctx

getExprContext :: GenericExpr -> GenericContext
getExprContext (EVar    ctx _    ) = ctx
getExprContext (ELitInt ctx _    ) = ctx
getExprContext (ELitTrue  ctx    ) = ctx
getExprContext (ELitFalse ctx    ) = ctx
getExprContext (EApp ctx _ _     ) = ctx
getExprContext (EString ctx _    ) = ctx
getExprContext (Neg     ctx _    ) = ctx
getExprContext (Not     ctx _    ) = ctx
getExprContext (EMul ctx _ _ _   ) = ctx
getExprContext (EAdd ctx _ _ _   ) = ctx
getExprContext (ERel ctx _ _ _   ) = ctx
getExprContext (EAnd ctx _ _     ) = ctx
getExprContext (EOr  ctx _ _     ) = ctx
getExprContext (ELambda ctx _ _ _) = ctx

typeWithContext (Int  ctx   ) = (ctx, "integer")
typeWithContext (Str  ctx   ) = (ctx, "string")
typeWithContext (Bool ctx   ) = (ctx, "boolean")
typeWithContext (Void ctx   ) = (ctx, "void")
typeWithContext (Fun ctx _ _) = (ctx, "function")
