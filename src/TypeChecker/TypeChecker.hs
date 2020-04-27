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

ensureType :: SType -> SType -> TypeCheckerMonad SType
ensureType expected actual = ensureAnyType actual [expected]

ensureAnyType :: SType -> [SType] -> TypeCheckerMonad SType
ensureAnyType t1 t2 = do
  stmt_ <- asks stmt
  unless (t1 `elem` t2) . throwError $ TypeMismatch t1 t2 stmt_
  return t1

ensureReturnType :: Maybe SType -> Maybe SType -> TypeCheckerMonad ()
ensureReturnType t1 t2 = do
  let maybePair = (,) <$> t1 <*> t2
  case maybePair of
    (Just (fT, sT)) -> ensureType fT sT >> return ()
    _               -> return ()

ensureExprTypes :: Expr -> Expr -> [SType] -> TypeCheckerMonad SType
ensureExprTypes e1 e2 possibleTypes = do
  typeE1 <- typeCheckExpr e1
  typeE2 <- typeCheckExpr e2
  ensureType typeE1 typeE2
  ensureAnyType typeE1 possibleTypes

getOrError :: Maybe a -> TypeError -> TypeCheckerMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

varPossibleTypes :: [SType]
varPossibleTypes = [Int, Str, Bool]
comparableTypes = varPossibleTypes
functionReturnTypes = varPossibleTypes ++ [Void]
possiblePrintTypes = varPossibleTypes

ensureLoopExists :: Stmt -> Env -> TypeCheckerMonad ()
ensureLoopExists s e =
  unless (loopsOnStack e > 0) . throwError $ OutsideOfLoop s

checkFunction :: SType -> [Expr] -> Ident -> TypeCheckerMonad SType
checkFunction (Fun retType args) exprs name = do
  ensureLength args exprs
  passedArgs <- sequence $ map typeCheckExpr exprs
  let passedWithExpr       = args `zip` exprs
  let passedWithActualType = passedArgs `zip` (map maybeRefToSType args)
  sequence_ $ map (uncurry ensureType) passedWithActualType
  sequence_ $ map (uncurry ensureReference) passedWithExpr
  return retType
 where
  ensureReference :: MaybeRefType -> Expr -> TypeCheckerMonad ()
  ensureReference (JustRef t) (EVar x) = return ()
  ensureReference (JustRef _) _        = do
    stmt_ <- asks stmt
    throwError $ BadReference stmt_
  ensureReference _ _ = return ()

  ensureLength :: [a] -> [b] -> TypeCheckerMonad ()
  ensureLength t1 t2 =
    unless (length t1 == length t2) . throwError $ WrongNumberOfArguments name
checkFunction _ _ _ = throwError NotAFunction

typeCheckExpr :: Expr -> TypeCheckerMonad SType
typeCheckExpr expr = putStmt (SExp expr) $ go expr
 where
  go (ELambda args ret block) = do
    typeCheckStmtM (SFnDef ret (Ident "[]") args block)
    return $ Fun ret (map argToRefType args)
  go (ELitInt _            ) = return Int
  go (ELitTrue             ) = return Bool
  go (ELitFalse            ) = return Bool
  go (EString _            ) = return Str
  go (Neg     v            ) = typeCheckExpr v >>= ensureType (Int)
  go (Not     v            ) = typeCheckExpr v >>= ensureType (Bool)
  go (EMul left op    right) = ensureExprTypes left right [Int]
  go (EAdd left Plus  right) = ensureExprTypes left right [Int, Str]
  go (EAdd left Minus right) = ensureExprTypes left right [Int]
  go (EAnd left right      ) = ensureExprTypes left right [Bool]
  go (EOr  left right      ) = ensureExprTypes left right [Bool]
  go (ERel left _ right) =
    ensureExprTypes left right comparableTypes >> return Bool
  go (EVar v) = do
    env     <- asks types
    context <- asks stmt
    getOrError (M.lookup v env) (NotInitialized v context)
  go (EApp name exprs) = do
    env     <- asks types
    context <- asks stmt
    t       <- getOrError (M.lookup name env) (NotInitialized name context)
    checkFunction t exprs name

correct :: TypeCheckerMonad Env
correct = do
  env <- ask
  return $ overrideStatus Nothing env

typeCheckStmtM :: Stmt -> TypeCheckerMonad Env
typeCheckStmtM stmt = putStmt stmt $ go stmt
 where
  go (Ret expr) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureReturnType (funRetType env) (Just t)
    return $ overrideJustStatus t env

  go (VRet) = do
    env <- ask
    ensureReturnType (funRetType env) (Just Void)
    return $ overrideJustStatus Void env

  go (Empty) = correct

  go (Print expr) =
    typeCheckExpr expr >>= flip ensureAnyType possiblePrintTypes >> correct

  go (    SBreak       ) = ask >>= ensureLoopExists SBreak >> correct

  go (    SContinue    ) = ask >>= ensureLoopExists SContinue >> correct

  go ctx@(Ass name expr) = do
    env <- asks types
    t   <- getOrError (M.lookup name env) (NotInitialized name ctx)
    val <- typeCheckExpr expr
    ensureType t val
    correct

  go (Incr name     ) = typeCheckStmtM (Ass name (ELitInt 1))

  go (Decr name     ) = typeCheckStmtM (Incr name)

  go (SExp expr     ) = typeCheckExpr expr >> correct

  go (Cond expr stmt) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureType Bool t
    blockEnv <- local (putBlockOnStack) $ typeCheckStmtM stmt
    return $ alternateStatus (status blockEnv) env

  go (CondElse expr stmt1 stmt2) = do
    env <- ask
    t   <- typeCheckExpr expr
    ensureType Bool t
    blockEnv1 <- local (putBlockOnStack) $ typeCheckStmtM stmt1
    blockEnv2 <- local (putBlockOnStack) $ typeCheckStmtM stmt2
    ensureReturnType (status blockEnv1) (status blockEnv2)
    return $ alternateStatus (status blockEnv2)
                             (alternateStatus (status blockEnv1) env)

  go (Decl _ []                  ) = ask

  go (Decl t ((NoInit name) : xs)) = do
    env <- ask >>= putType name t
    local (const env) $ typeCheckStmtM (Decl t xs)

  go (Decl t ((Init name expr) : xs)) = do
    curEnv <- ask
    val    <- typeCheckExpr expr
    ensureType t val
    env <- putType name t curEnv
    local (const env) $ typeCheckStmtM (Decl t xs)

  go (SFnDef retType name args block) = do
    entireEnv <- ask
    sequence_ $ fmap ensureArgumentType args
    let maybeRefArgType = map argToRefType args
    envAfterDecl <- local (putBlockOnStack)
      $ typeCheckStmtsM (map argToNoInit args)
    putFun <- putType name (Fun retType maybeRefArgType) envAfterDecl
    let putRetType = putFunRetType retType putFun
    let blockOut   = removeBlockFromStack putRetType
    envBody  <- local (const blockOut) $ typeCheckStmtM (BStmt block)
    bodyType <- getOrError (status envBody)
                           (FunctionBodyDoesNotReturnValue name)
    ensureType retType bodyType
    putType name (Fun retType maybeRefArgType) entireEnv

  go (BStmt (Block stmts)) = do
    env      <- ask
    blockEnv <- local (putBlockOnStack) $ typeCheckStmtsM stmts
    return $ alternateStatus (status blockEnv) env

  go (While expr stmt) = do
    env   <- ask
    exprT <- typeCheckExpr expr
    ensureType exprT Bool
    blockEnv <- local (putLoopOnStack) $ typeCheckStmtM stmt
    return $ alternateStatus (status blockEnv) env

  ensureArgumentType :: Arg -> TypeCheckerMonad ()
  ensureArgumentType (Arg    (Fun _ _) _) = pure ()
  ensureArgumentType (RefArg (Fun _ _) _) = throwError FunctionNotReferenceable
  ensureArgumentType t =
    ensureAnyType (argToSType t) varPossibleTypes >> pure ()



typeCheckStmtsM :: [Stmt] -> TypeCheckerMonad Env
typeCheckStmtsM []       = ask
typeCheckStmtsM (x : []) = typeCheckStmtM x
typeCheckStmtsM (x : xs) = do
  env <- typeCheckStmtM x
  local (const $ alternateFunRetType (status env) env) $ typeCheckStmtsM xs
