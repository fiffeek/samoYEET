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
    status :: Maybe SType
    }

putType :: Ident -> SType -> Env -> Env
putType ident t env =
  Env { types = M.insert ident t (types env), status = status env }

alternateStatus :: Maybe SType -> Env -> Env
alternateStatus s e = Env { types = types e, status = status e <|> s }

overrideStatus :: Maybe SType -> Env -> Env
overrideStatus s e = Env { types = types e, status = s }

overrideJustStatus :: SType -> Env -> Env
overrideJustStatus s e = overrideStatus (Just s) e

data TypeError = NotAFunction | TypeMismatch SType [SType] | NotInitialized Ident | UnknownError deriving Show

type TypeCheckerMonad a = ReaderT Env (ExceptT TypeError IO) a

initialEnvironment :: Env
initialEnvironment = Env { types = M.empty, status = Nothing }

execTypeCheckerMonad :: [Stmt] -> IO ()
execTypeCheckerMonad = runTypeCheckerMonad initialEnvironment . typeCheckStmtsM

errorsHandler :: TypeError -> IO ()
errorsHandler error = putStrLn . addPrefix . go $ error
 where
  addPrefix = (++) "Type error: "
  go (TypeMismatch actual expected) =
    "expected = [" ++ (show expected) ++ "] actual = [" ++ (show actual) ++ "]"
  go (NotInitialized idn) = "variable = [" ++ (show idn) ++ "] not initialized"

runTypeCheckerMonad :: Env -> TypeCheckerMonad Env -> IO ()
runTypeCheckerMonad env m = do
  ans <- e
  case ans of
    Left  err -> errorsHandler $ err
    Right _   -> do
      putStrLn $ "Wegood"
      pure ()
 where
  r = runReaderT m env
  e = runExceptT r

checkTypesMatch :: SType -> SType -> TypeCheckerMonad SType
checkTypesMatch t1 t2 = checkAnyTypeMatch t1 [t2]

checkAnyTypeMatch :: SType -> [SType] -> TypeCheckerMonad SType
checkAnyTypeMatch t1 t2 = do
  unless (t1 `elem` t2) . throwError $ TypeMismatch t1 t2
  return t1

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

checkFunction :: SType -> TypeCheckerMonad SType
checkFunction (Fun retType args) = do
  checkAnyTypeMatch retType functionReturnTypes
  sequence $ map (flip checkAnyTypeMatch varPossibleTypes) argsToType
  return retType
 where
  argToType (NoRef   t) = t
  argToType (JustRef t) = t
  argsToType = map argToType args
checkFunction _ = throwError NotAFunction

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
typeCheckExpr (ERel left _ right) = typeCheckExprEq left right comparableTypes
typeCheckExpr (EAnd left right      ) = typeCheckExprEq left right [Bool]
typeCheckExpr (EOr  left right      ) = typeCheckExprEq left right [Bool]
typeCheckExpr (EVar v               ) = do
  env <- asks types
  t   <- getOrError (M.lookup v env) (NotInitialized v)
  checkAnyTypeMatch t varPossibleTypes
typeCheckExpr (EApp name exprs) = do
  env <- asks types
  t   <- getOrError (M.lookup name env) (NotInitialized name)
  checkFunction t

correct :: TypeCheckerMonad Env
correct = do
  env <- ask
  return $ overrideStatus Nothing env

typeCheckStmtM :: Stmt -> TypeCheckerMonad Env
typeCheckStmtM (Ret expr) =
  liftM2 overrideJustStatus (typeCheckExpr expr) (ask)
typeCheckStmtM (VRet         ) = liftM2 overrideJustStatus (return Void) (ask)
typeCheckStmtM (Empty        ) = correct
typeCheckStmtM (Print _      ) = correct
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
  t <- typeCheckExpr expr
  checkTypesMatch t Bool
  typeCheckStmtM stmt
typeCheckStmtM (Decl _ []                  ) = ask
typeCheckStmtM (Decl t ((NoInit name) : xs)) = do
  local (putType name t) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (Decl t ((Init name expr) : xs)) = do
  val <- typeCheckExpr expr
  checkTypesMatch t val
  local (putType name t) $ typeCheckStmtM (Decl t xs)
typeCheckStmtM (SFnDef retType name args block) = undefined
typeCheckStmtM _ = correct

typeCheckStmtsM :: [Stmt] -> TypeCheckerMonad Env
typeCheckStmtsM []       = ask
typeCheckStmtsM (x : []) = typeCheckStmtM x
typeCheckStmtsM (x : xs) = do
  env <- typeCheckStmtM x
  statementValueDispatcher env (typeCheckStmtsM xs)

statementValueDispatcher :: Env -> TypeCheckerMonad Env -> TypeCheckerMonad Env
statementValueDispatcher env@(Env _ Nothing) stmts = local (const env) stmts
statementValueDispatcher env                 _     = return env
