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

type MemAdr = Int
data Store = Store {
    storage ::  M.Map MemAdr SType,
    freeAddresses :: [MemAdr]
}
type VEnv = M.Map Ident MemAdr
type PEnv = M.Map Ident FunctionDefinition

data Env = Env {
  vEnv :: VEnv,
  pEnv :: PEnv
}

data FunctionDefinition = FunctionDefinition {
  pType :: SType,
  ident :: Ident,
  pArgs :: [Arg],
  body :: Block,
  env :: Env
}

data RuntimeError = UnknownError deriving Show

type InterpretMonad a
  = ReaderT Env (CMS.StateT Store (ExceptT RuntimeError IO)) a


initialEnvironment :: Env
initialEnvironment = Env { vEnv = M.empty, pEnv = M.empty }

initialState :: Store
initialState = Store { storage = M.empty, freeAddresses = [1 ..] }
