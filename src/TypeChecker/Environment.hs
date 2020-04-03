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

type TypeCheckerMonad a = ReaderT Env (ExceptT TypeError IO) a

initialEnvironment :: Env
initialEnvironment = Env { types        = M.empty
                         , status       = Nothing
                         , funRetType   = Nothing
                         , loopsOnStack = 0
                         }

