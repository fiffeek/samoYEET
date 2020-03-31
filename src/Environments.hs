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
  pEnv :: PEnv
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
initialEnvironment = Env { vEnv = M.empty, pEnv = M.empty }

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

putValInEnv :: Ident -> MemAdr -> Env -> Env
putValInEnv name addr env =
  Env { vEnv = M.insert name addr (vEnv env), pEnv = pEnv env }
