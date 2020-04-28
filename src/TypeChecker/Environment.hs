module TypeChecker.Environment where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative
import           TypeChecker.TypeError
import           TypeChecker.Types

data Env = Env {
    types :: M.Map Ident TCSType,
    scope :: M.Map Ident Int,
    status :: Maybe TCSType,
    funRetType :: Maybe TCSType,
    loopsOnStack :: Int,
    blocksOnStack :: Int,
    stmt :: TCStmt
    } deriving Show


putStmt :: TCStmt -> TypeCheckerMonad a -> TypeCheckerMonad a
putStmt stmt_ cont = local (\e -> e { stmt = stmt_ }) cont

putType :: Ident -> TCSType -> Env -> TypeCheckerMonad Env
putType ident t env = do
  let maybeType  = M.lookup ident (types env)
  let maybeScope = M.lookup ident (scope env)
  let tupled     = (,) <$> maybeType <*> maybeScope
  stmt_ <- asks stmt
  case (tupled, (t, blocksOnStack env)) of
    ((Just (t1, s1)), (t2, s2)) -> do
      when (and [s1 == s2, t1 /= t2])
        $ throwError (ConflictingDeclarations ident stmt_)
      when (and [s1 == s2, t1 == t2]) $ throwError (Redeclaration ident stmt_)
    _ -> return ()
  return updatedEnv
 where
  updatedEnv = env { types = M.insert ident t (types env)
                   , scope = M.insert ident (blocksOnStack env) (scope env)
                   }


alternateFunRetType :: Maybe TCSType -> Env -> Env
alternateFunRetType t e = e { funRetType = t <|> funRetType e }

putFunRetType :: TCSType -> Env -> Env
putFunRetType t1 e = e { funRetType = Just t1 }

putBlockOnStack :: Env -> Env
putBlockOnStack env = env { blocksOnStack = (+) 1 $ blocksOnStack env }

removeBlockFromStack :: Env -> Env
removeBlockFromStack env = env { blocksOnStack = (-) 1 $ blocksOnStack env }

alternateStatus :: Maybe TCSType -> Env -> Env
alternateStatus s e = e { status = status e <|> s }

overrideStatus :: Maybe TCSType -> Env -> Env
overrideStatus s e = e { status = s }

overrideJustStatus :: TCSType -> Env -> Env
overrideJustStatus s e = overrideStatus (Just s) e

putLoopOnStack :: Env -> Env
putLoopOnStack env = env { loopsOnStack = (+) 1 $ loopsOnStack env }

type TypeCheckerMonad a = ReaderT Env (ExceptT TypeError IO) a

initialEnvironment :: Env
initialEnvironment = Env { types         = M.empty
                         , scope         = M.empty
                         , status        = Nothing
                         , funRetType    = Just (Int Nothing)
                         , loopsOnStack  = 0
                         , blocksOnStack = 0
                         , stmt          = Empty Nothing
                         }

