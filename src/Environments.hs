module Environments where

import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Control.Monad.State           as CMS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs                   ( Arg
                                                , Block
                                                , SType
                                                , Ident
                                                )
import           ValueTypes
import           Utils
import           RuntimeError

type MemAdr = Int
data Store = Store {
    storage ::  M.Map MemAdr VType,
    freeAddresses :: [MemAdr]
}
type VEnv = M.Map Ident MemAdr
type PEnv = M.Map Ident FunctionDefinition

data Env = Env {
  vEnv :: VEnv,
  pEnv :: PEnv,
  vtype :: VType
} deriving Show

data FunctionDefinition = FunctionDefinition {
  pType :: SType,
  ident :: Ident,
  pArgs :: [Arg],
  body :: Block,
  env :: Env
} deriving Show

type InterpretMonad a
  = ReaderT Env (CMS.StateT Store (ExceptT RuntimeError IO)) a


initialEnvironment :: Env
initialEnvironment = Env { vEnv = M.empty, pEnv = M.empty, vtype = VNone }

initialState :: Store
initialState = Store { storage = M.empty, freeAddresses = [1 ..] }

getStorage :: InterpretMonad Store -> InterpretMonad (M.Map MemAdr VType)
getStorage = fmap storage

getFreeAddr :: Store -> (MemAdr, [MemAdr])
getFreeAddr store = (head freeAddrs, tail freeAddrs)
  where freeAddrs = freeAddresses store

putValInStore :: VType -> (MemAdr, [MemAdr]) -> Store -> Store
putValInStore val (freeAddr, rest) s =
  Store { storage = M.insert freeAddr val (storage s), freeAddresses = rest }

putValInAddr :: VType -> MemAdr -> Store -> Store
putValInAddr val addr s = Store { storage       = M.insert addr val (storage s)
                                , freeAddresses = freeAddresses s
                                }

putValInEnv :: Ident -> MemAdr -> Env -> Env
putValInEnv name addr env = Env { vEnv  = M.insert name addr (vEnv env)
                                , pEnv  = pEnv env
                                , vtype = vtype env
                                }

changeRetType :: VType -> Env -> Env
changeRetType v env = Env { vEnv = vEnv env, pEnv = pEnv env, vtype = v }

flushVariables :: Env -> Env -> Env
flushVariables env1 env2 =
  Env { vEnv = vEnv env1, pEnv = pEnv env1, vtype = vtype env2 }

putFunInEnv :: SType -> Ident -> [Arg] -> Block -> Env -> Env
putFunInEnv t n args b env = Env { vEnv  = vEnv env
                                 , pEnv  = M.insert n makeFunc (pEnv env)
                                 , vtype = vtype env
                                 }
 where
  makeFunc = FunctionDefinition
    { pType = t
    , ident = n
    , pArgs = args
    , body  = b
    , env   = Env { vEnv  = vEnv env
                  , pEnv  = M.insert n makeFunc (pEnv env)
                  , vtype = vtype env
                  }
    }
