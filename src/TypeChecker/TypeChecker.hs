module TypeChecker.TypeChecker where
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


checkTypesMatch :: SType -> SType -> TypeCheckerMonad SType
checkTypesMatch t1 t2 = checkAnyTypeMatch t1 [t2]

checkAnyTypeMatch :: SType -> [SType] -> TypeCheckerMonad SType
checkAnyTypeMatch t1 t2 = do
  unless (t1 `elem` t2) . throwError $ TypeMismatch t1 t2
  return t1

checkReturnTypes :: Maybe SType -> Maybe SType -> TypeCheckerMonad ()
checkReturnTypes t1 t2 = do
  let maybePair = (,) <$> t1 <*> t2
  case maybePair of
    (Just (fT, sT)) -> checkTypesMatch fT sT >> return ()
    _               -> return ()

checkSameLength :: [a] -> [b] -> TypeCheckerMonad ()
checkSameLength t1 t2 = do
  unless (length t1 == length t2) . throwError $ WrongNumberOfArguments

typeCheckExprEq :: Expr -> Expr -> [SType] -> TypeCheckerMonad SType
typeCheckExprEq e1 e2 possibleTypes = do
  typeE1 <- typeCheckExpr e1
  typeE2 <- typeCheckExpr e2
  checkTypesMatch typeE1 typeE2
  checkAnyTypeMatch typeE1 possibleTypes

getOrError :: Maybe a -> TypeError -> TypeCheckerMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

varPossibleTypes :: [SType]
varPossibleTypes = [Int, Str, Bool]
comparableTypes = varPossibleTypes
functionReturnTypes = varPossibleTypes ++ [Void]
possiblePrintTypes = varPossibleTypes

checkLoopsOnStack :: Stmt -> Env -> TypeCheckerMonad ()
checkLoopsOnStack s e = do
  unless (loopsOnStack e > 0) . throwError $ OutsideOfLoop s

checkFunctionArgument :: Arg -> TypeCheckerMonad ()
checkFunctionArgument (Arg (Fun _ _) _) = pure ()
checkFunctionArgument (RefArg (Fun _ _) _) =
  throwError FunctionNotReferenceable
checkFunctionArgument t =
  checkAnyTypeMatch (argToSType t) varPossibleTypes >> pure ()

checkFunction :: SType -> [SType] -> TypeCheckerMonad SType
checkFunction (Fun retType args) passedArgs = do
  checkSameLength args passedArgs
  let passedWithActualType = passedArgs `zip` (map maybeRefToSType args)
  sequence_ $ map (uncurry checkTypesMatch) passedWithActualType
  return retType
checkFunction _ _ = throwError NotAFunction

typeCheckExpr :: Expr -> TypeCheckerMonad SType
typeCheckExpr (ELitInt _             ) = return Int
typeCheckExpr (ELitTrue              ) = return Bool
typeCheckExpr (ELitFalse             ) = return Bool
typeCheckExpr (EString _             ) = return Str
typeCheckExpr (ELambda args ret block) = do
  typeCheckStmtM (SFnDef ret (Ident "[]") args block)
  return $ Fun ret (map argToRefType args)
typeCheckExpr (Neg v) = do
  t <- typeCheckExpr v
  checkTypesMatch (Int) t
typeCheckExpr (Not v) = do
  t <- typeCheckExpr v
  checkTypesMatch (Bool) t
typeCheckExpr (EMul left op    right) = typeCheckExprEq left right [Int]
typeCheckExpr (EAdd left Plus  right) = typeCheckExprEq left right [Int, Str]
typeCheckExpr (EAdd left Minus right) = typeCheckExprEq left right [Int]
typeCheckExpr (ERel left _     right) = do
  typeCheckExprEq left right comparableTypes
  return Bool
typeCheckExpr (EAnd left right) = typeCheckExprEq left right [Bool]
typeCheckExpr (EOr  left right) = typeCheckExprEq left right [Bool]
typeCheckExpr (EVar v         ) = do
  env <- asks types
  getOrError (M.lookup v env) (NotInitialized v)
typeCheckExpr (EApp name exprs) = do
  env    <- asks types
  t      <- getOrError (M.lookup name env) (NotInitialized name)
  exprsT <- sequence $ map typeCheckExpr exprs
  checkFunction t exprsT

correct :: TypeCheckerMonad Env
correct = do
  env <- ask
  return $ overrideStatus Nothing env

typeCheckStmtM :: Stmt -> TypeCheckerMonad Env
typeCheckStmtM (Ret expr) = do
  env <- ask
  t   <- typeCheckExpr expr
  checkReturnTypes (funRetType env) (Just t)
  return $ overrideJustStatus t env
typeCheckStmtM (VRet) = do
  env <- ask
  checkReturnTypes (funRetType env) (Just Void)
  return $ overrideJustStatus Void env
typeCheckStmtM (Empty     ) = correct
typeCheckStmtM (Print expr) = do
  t <- typeCheckExpr expr
  checkAnyTypeMatch t possiblePrintTypes
  correct
typeCheckStmtM (SBreak) = do
  env <- ask
  checkLoopsOnStack SBreak env >> correct
typeCheckStmtM (SContinue) = do
  env <- ask
  checkLoopsOnStack SContinue env >> correct
typeCheckStmtM (Ass name expr) = do
  env <- asks types
  t   <- getOrError (M.lookup name env) (NotInitialized name)
  val <- typeCheckExpr expr
  checkTypesMatch t val
  correct
typeCheckStmtM (Incr name     ) = typeCheckStmtM (Ass name (ELitInt 1))
typeCheckStmtM (Decr name     ) = typeCheckStmtM (Incr name)
typeCheckStmtM (SExp expr     ) = typeCheckExpr expr >> correct
typeCheckStmtM (Cond expr stmt) = do
  env <- ask
  t   <- typeCheckExpr expr
  checkTypesMatch t Bool
  blockEnv <- local (putBlockOnStack) $ typeCheckStmtM stmt
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM (CondElse expr stmt1 stmt2) = do
  env <- ask
  t   <- typeCheckExpr expr
  checkTypesMatch t Bool
  blockEnv1 <- local (putBlockOnStack) $ typeCheckStmtM stmt1
  blockEnv2 <- local (putBlockOnStack) $ typeCheckStmtM stmt2
  checkReturnTypes (status blockEnv1) (status blockEnv2)
  return $ alternateStatus (status blockEnv2)
                           (alternateStatus (status blockEnv1) env)
typeCheckStmtM (Decl _ []                  ) = ask
typeCheckStmtM (Decl t ((NoInit name) : xs)) = do
  curEnv <- ask
  env    <- putType name t curEnv
  local (const env) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (Decl t ((Init name expr) : xs)) = do
  curEnv <- ask
  val    <- typeCheckExpr expr
  checkTypesMatch t val
  env <- putType name t curEnv
  local (const env) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (SFnDef retType name args block) = do
  entireEnv <- ask
  sequence_ $ fmap checkFunctionArgument args
  let maybeRefArgType = map argToRefType args
  envAfterDecl <- local (putBlockOnStack)
    $ typeCheckStmtsM (map argToNoInit args)
  putFun <- putType name (Fun retType maybeRefArgType) envAfterDecl
  let putRetType = putFunRetType retType putFun
  envBody  <- local (const putRetType) $ typeCheckStmtM (BStmt block)
  bodyType <- getOrError (status envBody) (FunctionBodyDoesNotReturnValue)
  checkTypesMatch retType bodyType
  putType name (Fun retType maybeRefArgType) entireEnv
typeCheckStmtM (BStmt (Block stmts)) = do
  env      <- ask
  blockEnv <- local (putBlockOnStack) $ typeCheckStmtsM stmts
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM (While expr stmt) = do
  env   <- ask
  exprT <- typeCheckExpr expr
  checkTypesMatch exprT Bool
  blockEnv <- local (putLoopOnStack) $ typeCheckStmtM stmt
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM _ = correct

typeCheckStmtsM :: [Stmt] -> TypeCheckerMonad Env
typeCheckStmtsM []       = ask
typeCheckStmtsM (x : []) = typeCheckStmtM x
typeCheckStmtsM (x : xs) = do
  env <- typeCheckStmtM x
  local (const $ alternateFunRetType (status env) env) $ typeCheckStmtsM xs
