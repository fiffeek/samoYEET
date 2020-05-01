module TypeChecker.TypeChecker
  ( typeCheckStmtsM
  )
where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative
import           TypeChecker.Converter
import           TypeChecker.Environment
import           TypeChecker.TypeError
import           TypeChecker.Types
import           Common.Types

ensureType
  :: TypeMismatchContext -> TCSType -> TCSType -> TypeCheckerMonad TCSType
ensureType ctx expected actual = ensureAnyType ctx actual [expected]

ensureAnyType
  :: TypeMismatchContext -> TCSType -> [TCSType] -> TypeCheckerMonad TCSType
ensureAnyType ctx t1 t2 = do
  stmt_ <- asks stmt
  unless (t1NoContext `elem` t2NoContext) . throwError $ TypeMismatch ctx
                                                                      t1
                                                                      t2
                                                                      stmt_
  return t1
 where
  t1NoContext = dropContext t1
  t2NoContext = fmap dropContext t2

ensureReturnType :: Maybe TCSType -> Maybe TCSType -> TypeCheckerMonad ()
ensureReturnType t1 t2 = do
  let maybePair = (,) <$> t1 <*> t2
  case maybePair of
    (Just (fT, sT)) -> ensureType (ReturnType fT) fT sT >> return ()
    _               -> return ()

ensureExprTypes
  :: TypeMismatchContext
  -> TCExpr
  -> TCExpr
  -> [TCSType]
  -> TypeCheckerMonad TCSType
ensureExprTypes ctx e1 e2 possibleTypes = do
  typeE1 <- typeCheckExpr e1
  typeE2 <- typeCheckExpr e2
  ensureAnyType ctx typeE1 possibleTypes
  ensureType LHSRHS typeE1 typeE2

getOrError :: Maybe a -> TypeError -> TypeCheckerMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

varPossibleTypes :: [TCSType]
varPossibleTypes = [Int, Str, Bool] <*> [Nothing]
comparableTypes = varPossibleTypes
functionReturnTypes = varPossibleTypes ++ [Void Nothing]
possiblePrintTypes = varPossibleTypes

ensureLoopExists :: TCStmt -> Env -> TypeCheckerMonad ()
ensureLoopExists s e =
  unless (loopsOnStack e > 0) . throwError $ OutsideOfLoop s

checkFunction :: TCSType -> [TCExpr] -> Ident -> TypeCheckerMonad TCSType
checkFunction (Fun _ retType args) exprs name = do
  ensureLength args exprs
  passedArgs <- sequence $ map typeCheckExpr exprs
  let passedWithExpr       = args `zip` exprs
  let passedWithActualType = passedArgs `zip` (map maybeRefToSType args)
  sequence_ $ map (uncurry $ ensureType FunArgType) passedWithActualType
  sequence_ $ map (uncurry ensureReference) passedWithExpr
  return retType
 where
  ensureReference :: TCMaybeRefType -> TCExpr -> TypeCheckerMonad ()
  ensureReference (JustRef _ t) (EVar _ x) = return ()
  ensureReference (JustRef _ _) _          = do
    stmt_ <- asks stmt
    throwError $ BadReference stmt_
  ensureReference _ _ = return ()

  ensureLength :: [a] -> [b] -> TypeCheckerMonad ()
  ensureLength t1 t2 =
    unless (length t1 == length t2) . throwError $ WrongNumberOfArguments name
checkFunction _ _ _ = throwError NotAFunction

typeCheckExpr :: TCExpr -> TypeCheckerMonad TCSType
typeCheckExpr expr = putStmt (SExp (getExprContext expr) expr) $ go expr
 where
  go (ELambda ctx args ret block) = do
    typeCheckStmtM (SFnDef ctx ret (Ident "[]") args block)
    return $ Fun ctx ret (map argToRefType args)
  go (ELitInt ctx _         ) = return $ Int ctx
  go (ELitTrue  ctx         ) = return $ Bool ctx
  go (ELitFalse ctx         ) = return $ Bool ctx
  go (EString ctx _         ) = return $ Str ctx
  go (Neg ctx v) = typeCheckExpr v >>= ensureType BinaryInt (Int ctx)
  go (Not ctx v) = typeCheckExpr v >>= ensureType BinaryBool (Bool ctx)
  go (EMul ctx left op right) = ensureExprTypes OpInt left right [Int ctx]
  go (EAnd ctx left right   ) = ensureExprTypes OpBool left right [Bool ctx]
  go (EOr  ctx left right   ) = ensureExprTypes OpBool left right [Bool ctx]
  go (EAdd ctx left (Minus _) right) =
    ensureExprTypes OpInt left right [Int ctx]
  go (EAdd ctx left (Plus _) right) =
    ensureExprTypes OpPlus left right ([Int, Str] <*> [ctx])
  go (ERel ctx left _ right) =
    ensureExprTypes ComparableTypes left right comparableTypes
      >> return (Bool ctx)
  go (EVar _ v) = do
    env     <- asks types
    context <- asks stmt
    getOrError (M.lookup v env) (NotInitialized v context)
  go (EApp _ name exprs) = do
    env     <- asks types
    context <- asks stmt
    t       <- getOrError (M.lookup name env) (NotInitialized name context)
    checkFunction t exprs name

correct :: TypeCheckerMonad Env
correct = do
  env <- ask
  return $ overrideStatus Nothing env

typeCheckStmtM :: TCStmt -> TypeCheckerMonad Env
typeCheckStmtM stmt = putStmt stmt $ go stmt
 where
  go (Ret _ expr) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureReturnType (funRetType env) (Just t)
    return $ overrideJustStatus t env

  go (VRet ctx) = do
    env <- ask
    ensureReturnType (funRetType env) (Just (Void ctx))
    return $ overrideJustStatus (Void ctx) env

  go (Empty _) = correct

  go (Print _ expr) =
    typeCheckExpr expr
      >>= flip (ensureAnyType PrintT) possiblePrintTypes
      >>  correct

  go (    SBreak    ctx  ) = ask >>= ensureLoopExists (SBreak ctx) >> correct
  go (    SContinue ctx  ) = ask >>= ensureLoopExists (SContinue ctx) >> correct

  go ctx@(Ass _ name expr) = do
    env <- asks types
    t   <- getOrError (M.lookup name env) (NotInitialized name ctx)
    val <- typeCheckExpr expr
    ensureType (Assignment t) t val
    correct

  go (Incr ctx name     ) = typeCheckStmtM (Ass ctx name (ELitInt ctx 1))
  go (Decr ctx name     ) = typeCheckStmtM (Incr ctx name)
  go (SExp _   expr     ) = typeCheckExpr expr >> correct

  go (Cond ctx expr stmt) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureType ConditionBool (Bool ctx) t
    blockEnv <- local (putBlockOnStack) $ typeCheckStmtM stmt
    return $ alternateStatus (status blockEnv) env

  go (CondElse ctx expr stmt1 stmt2) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureType ConditionBool (Bool ctx) t
    blockEnv1 <- local (putBlockOnStack) $ typeCheckStmtM stmt1
    blockEnv2 <- local (putBlockOnStack) $ typeCheckStmtM stmt2
    ensureReturnType (status blockEnv1) (status blockEnv2)
    return $ alternateStatus (status blockEnv2)
                             (alternateStatus (status blockEnv1) env)

  go (Decl _   _ []                    ) = ask
  go (Decl ctx t ((NoInit _ name) : xs)) = do
    env <- ask >>= putType name t
    local (const env) $ typeCheckStmtM (Decl ctx t xs)
  go (Decl ctx t ((Init _ name expr) : xs)) = do
    curEnv <- ask
    val    <- typeCheckExpr expr
    ensureType (DeclVaries t) t val
    env <- putType name t curEnv
    local (const env) $ typeCheckStmtM (Decl ctx t xs)

  go funDef@(SFnDef ctx retType name args block) = do
    entireEnv <- ask
    sequence_ $ fmap (ensureArgumentType name funDef) args
    let maybeRefArgType = map argToRefType args
    envAfterDecl <- local (putBlockOnStack)
      $ typeCheckStmtsM (map argToNoInit args)
    let removeBlock = removeBlockFromStack envAfterDecl
    putFun <- putType name (Fun ctx retType maybeRefArgType) removeBlock
    let putRetType = putFunRetType retType putFun
    envBody  <- local (const putRetType) $ typeCheckStmtM (BStmt ctx block)
    bodyType <- getOrError (status envBody)
                           (FunctionBodyDoesNotReturnValue name)
    putType name (Fun ctx retType maybeRefArgType) entireEnv

  go (BStmt _ (Block _ stmts)) = do
    env      <- ask
    blockEnv <- local (putBlockOnStack) $ typeCheckStmtsM stmts
    return $ alternateStatus (status blockEnv) env

  go (While ctx expr stmt) = do
    env   <- ask
    exprT <- typeCheckExpr expr
    ensureType ConditionBool (Bool ctx) exprT
    blockEnv <- local (putLoopOnStack) $ typeCheckStmtM stmt
    return $ alternateStatus (status blockEnv) env

  ensureArgumentType :: Ident -> TCStmt -> TCArg -> TypeCheckerMonad ()
  ensureArgumentType _ _ (Arg _ (Fun _ _ _) _) = pure ()
  ensureArgumentType name ctx (RefArg _ (Fun _ _ _) _) =
    throwError $ FunctionNotReferenceable name ctx
  ensureArgumentType _ _ t =
    ensureAnyType FunArgType (argToSType t) varPossibleTypes >> pure ()



typeCheckStmtsM :: [TCStmt] -> TypeCheckerMonad Env
typeCheckStmtsM []       = ask
typeCheckStmtsM (x : []) = typeCheckStmtM x
typeCheckStmtsM (x : xs) = do
  env <- typeCheckStmtM x
  local (const $ alternateFunRetType (status env) env) $ typeCheckStmtsM xs
