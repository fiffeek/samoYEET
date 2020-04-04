module TypeChecker.Environment where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative
import           TypeChecker.TypeError

data Env = Env {
    types :: M.Map Ident SType,
    scope :: M.Map Ident Int,
    status :: Maybe SType,
    funRetType :: Maybe SType,
    loopsOnStack :: Int,
    blocksOnStack :: Int
    } deriving Show

putType :: Ident -> SType -> Env -> TypeCheckerMonad Env
putType ident t env = do
  let maybeType  = M.lookup ident (types env)
  let maybeScope = M.lookup ident (scope env)
  let tupled     = (,) <$> maybeType <*> maybeScope
  case (tupled, (t, blocksOnStack env)) of
    ((Just (t1, s1)), (t2, s2)) -> do
      when (and [s1 == s2, t1 /= t2]) $ throwError ConflictingDeclarations
      when (and [s1 == s2, t1 == t2]) $ throwError Redeclaration
    _ -> return ()
  return updatedEnv
 where
  updatedEnv = env { types = M.insert ident t (types env)
                   , scope = M.insert ident (blocksOnStack env) (scope env)
                   }


alternateFunRetType :: Maybe SType -> Env -> Env
alternateFunRetType t e = e { funRetType = t <|> funRetType e }

putFunRetType :: SType -> Env -> Env
putFunRetType t1 e = e { funRetType = Just t1 }

putBlockOnStack :: Env -> Env
putBlockOnStack env = env { blocksOnStack = (+) 1 $ blocksOnStack env }

alternateStatus :: Maybe SType -> Env -> Env
alternateStatus s e = e { status = status e <|> s }

overrideStatus :: Maybe SType -> Env -> Env
overrideStatus s e = e { status = s }

overrideJustStatus :: SType -> Env -> Env
overrideJustStatus s e = overrideStatus (Just s) e

putLoopOnStack :: Env -> Env
putLoopOnStack env = env { loopsOnStack = (+) 1 $ loopsOnStack env }

type TypeCheckerMonad a = ReaderT Env (ExceptT TypeError IO) a

initialEnvironment :: Env
initialEnvironment = Env { types         = M.empty
                         , scope         = M.empty
                         , status        = Nothing
                         , funRetType    = Just Int
                         , loopsOnStack  = 0
                         , blocksOnStack = 0
                         }

