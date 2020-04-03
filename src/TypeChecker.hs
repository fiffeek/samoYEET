module TypeChecker where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative

data Env = Env {
    types :: M.Map Ident SType,
    status :: Maybe SType,
    funRetType :: Maybe SType,
    loopsOnStack :: Int
    } deriving Show

putType :: Ident -> SType -> Env -> Env
putType ident t env = env { types = M.insert ident t (types env) }

alternateFunRetType :: Maybe SType -> Env -> Env
alternateFunRetType t e = e { funRetType = t <|> funRetType e }

putFunRetType :: SType -> Env -> Env
putFunRetType t1 e = e { funRetType = Just t1 }

alternateStatus :: Maybe SType -> Env -> Env
alternateStatus s e = e { status = status e <|> s }

overrideStatus :: Maybe SType -> Env -> Env
overrideStatus s e = e { status = s }

overrideJustStatus :: SType -> Env -> Env
overrideJustStatus s e = overrideStatus (Just s) e

addToLoops :: Int -> Env -> Env
addToLoops v e = e { loopsOnStack = (loopsOnStack e) + v }

incLoopsCtr :: Env -> Env
incLoopsCtr = addToLoops 1

decrLoopsCtr :: Env -> Env
decrLoopsCtr = addToLoops (-1)

data TypeError = NotAFunction
  | TypeMismatch SType [SType]
  | NotInitialized Ident
  | FunctionBodyDoesNotReturnValue
  | OutsideOfLoop Stmt
  | WrongNumberOfArguments
  | UnknownError deriving Show

type TypeCheckerMonad a = ReaderT Env (ExceptT TypeError IO) a

initialEnvironment :: Env
initialEnvironment = Env { types        = M.empty
                         , status       = Nothing
                         , funRetType   = Nothing
                         , loopsOnStack = 0
                         }

execTypeCheckerMonad :: [Stmt] -> IO () -> IO ()
execTypeCheckerMonad stmts =
  runTypeCheckerMonad initialEnvironment (typeCheckStmtsM stmts)

errorsHandler :: TypeError -> IO ()
errorsHandler error = putStrLn . addPrefix . go $ error
 where
  addPrefix = (++) "Type error: "
  go (TypeMismatch actual expected) =
    "expected = [" ++ (show expected) ++ "] actual = [" ++ (show actual) ++ "]"
  go (NotInitialized idn) = "variable = [" ++ (show idn) ++ "] not initialized"
  go WrongNumberOfArguments = "wrong number of arguments"
  go _ = "som error"

runTypeCheckerMonad :: Env -> TypeCheckerMonad Env -> IO () -> IO ()
runTypeCheckerMonad env m cont = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right _   -> cont
 where
  r = runReaderT m env
  e = runExceptT r

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

checkFunction :: SType -> [SType] -> TypeCheckerMonad SType
checkFunction (Fun retType args) passedArgs = do
  checkAnyTypeMatch retType functionReturnTypes
  sequence $ map (flip checkAnyTypeMatch varPossibleTypes) argsToType
  checkSameLength args passedArgs
  let passedWithActualType = passedArgs `zip` argsToType
  sequence_ $ map (\(x, y) -> checkTypesMatch x y) passedWithActualType
  return retType
 where
  argToType (NoRef   t) = t
  argToType (JustRef t) = t
  argsToType = map argToType args
checkFunction _ _ = throwError NotAFunction

typeCheckExpr :: Expr -> TypeCheckerMonad SType
typeCheckExpr (ELitInt _) = return Int
typeCheckExpr (ELitTrue ) = return Bool
typeCheckExpr (ELitFalse) = return Bool
typeCheckExpr (EString _) = return Str
typeCheckExpr (Neg     v) = do
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
  t   <- getOrError (M.lookup v env) (NotInitialized v)
  checkAnyTypeMatch t varPossibleTypes
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
  blockEnv <- typeCheckStmtM stmt
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM (CondElse expr stmt1 stmt2) = do
  env <- ask
  t   <- typeCheckExpr expr
  checkTypesMatch t Bool
  blockEnv1 <- typeCheckStmtM stmt1
  blockEnv2 <- typeCheckStmtM stmt2
  checkReturnTypes (status blockEnv1) (status blockEnv2)
  return $ alternateStatus (status blockEnv2)
                           (alternateStatus (status blockEnv1) env)
typeCheckStmtM (Decl _ []                  ) = ask
typeCheckStmtM (Decl t ((NoInit name) : xs)) = do
  local (putType name t) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (Decl t ((Init name expr) : xs)) = do
  val <- typeCheckExpr expr
  checkTypesMatch t val
  local (putType name t) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (SFnDef retType name args block) = do
  entireEnv <- ask
  env       <- asks types
  let argTypes = map argToSType args
  sequence_ $ fmap (flip checkAnyTypeMatch varPossibleTypes) argTypes
  let maybeRefArgType = map argToRefType args
  let decl            = map argToNoInit args
  envAfterDecl <- typeCheckStmtsM decl
  let putFun     = putType name (Fun retType maybeRefArgType) envAfterDecl
  let putRetType = putFunRetType retType putFun
  envBody  <- local (const putRetType) $ typeCheckStmtM (BStmt block)
  bodyType <- getOrError (status envBody) (FunctionBodyDoesNotReturnValue)
  checkTypesMatch retType bodyType
  return $ putType name (Fun retType maybeRefArgType) entireEnv
typeCheckStmtM (BStmt (Block stmts)) = do
  env      <- ask
  blockEnv <- typeCheckStmtsM stmts
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM (While expr stmt) = do
  env   <- ask
  exprT <- typeCheckExpr expr
  checkTypesMatch exprT Bool
  blockEnv <- local (incLoopsCtr) $ typeCheckStmtM stmt
  return $ alternateStatus (status blockEnv) env
typeCheckStmtM _ = correct

argToSType :: Arg -> SType
argToSType (Arg    t _) = t
argToSType (RefArg t _) = t

argToRefType :: Arg -> MaybeRefType
argToRefType (Arg    t _) = NoRef t
argToRefType (RefArg t _) = JustRef t

argToNoInit :: Arg -> Stmt
argToNoInit (Arg    t n) = Decl t [NoInit n]
argToNoInit (RefArg t n) = Decl t [NoInit n]

typeCheckStmtsM :: [Stmt] -> TypeCheckerMonad Env
typeCheckStmtsM []       = ask
typeCheckStmtsM (x : []) = typeCheckStmtM x
typeCheckStmtsM (x : xs) = do
  env <- typeCheckStmtM x
  local (const $ alternateFunRetType (status env) env) $ typeCheckStmtsM xs
