module Interpreter.Environments where

import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Control.Monad.State           as CMS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Interpreter.ValueTypes
import           Interpreter.RuntimeError

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

putVal :: Ident -> VType -> InterpretMonad Env
putVal name val = do
  env   <- ask
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  CMS.modify (putValInStore val splitState)
  return $ putValInEnv name loc env

putValInStore :: VType -> (MemAdr, [MemAdr]) -> Store -> Store
putValInStore val (freeAddr, rest) s =
  Store { storage = M.insert freeAddr val (storage s), freeAddresses = rest }

putValInAddr :: VType -> MemAdr -> Store -> Store
putValInAddr val addr s = Store { storage       = M.insert addr val (storage s)
                                , freeAddresses = freeAddresses s
                                }
updateFreeAddrs :: [MemAdr] -> Store -> Store
updateFreeAddrs rest s = Store { storage = storage s, freeAddresses = rest }

putValNoInit :: Ident -> InterpretMonad Env
putValNoInit name = do
  env   <- ask
  state <- CMS.get
  let (loc, rest) = getFreeAddr state
  CMS.modify (updateFreeAddrs rest)
  return $ putValInEnv name loc env

putValInEnv :: Ident -> MemAdr -> Env -> Env
putValInEnv name addr env = Env { vEnv  = M.insert name addr (vEnv env)
                                , pEnv  = M.delete name (pEnv env)
                                , vtype = vtype env
                                }

changeRetType :: VType -> Env -> Env
changeRetType v env = Env { vEnv = vEnv env, pEnv = pEnv env, vtype = v }

flushVariables :: Env -> Env -> Env
flushVariables env1 env2 =
  Env { vEnv = vEnv env1, pEnv = pEnv env1, vtype = vtype env2 }

putFunInEnv :: SType -> Ident -> [Arg] -> Block -> Env -> Env
putFunInEnv t n args b env = env { vEnv = M.delete n (vEnv env)
                                 , pEnv = M.insert n makeFunc (pEnv env)
                                 }
 where
  makeFunc = FunctionDefinition
    { pType = t
    , ident = n
    , pArgs = args
    , body  = b
    , env   = Env { vEnv  = M.delete n (vEnv env)
                  , pEnv  = M.insert n makeFunc (pEnv env)
                  , vtype = vtype env
                  }
    }

declareFunWithEnv :: SType -> Ident -> [Arg] -> Block -> Env -> Env -> Env
declareFunWithEnv t n args b fenv env = env
  { vEnv = M.delete n (vEnv env)
  , pEnv = M.insert n makeFunc (pEnv env)
  }
 where
  makeFunc = FunctionDefinition { pType = t
                                , ident = n
                                , pArgs = args
                                , body  = b
                                , env   = fenv
                                }

funDefToVType :: FunctionDefinition -> VType
funDefToVType (FunctionDefinition ret _ args body env) =
  VFun { fArgs = args, fBody = body, fEnv = env, fRet = ret }

getById :: Ident -> InterpretMonad VType
getById ident = do
  env <- ask
  let var = M.lookup ident (vEnv env)
  let f   = M.lookup ident (pEnv env)
  case (var, f) of
    ((Just loc), _) -> do
      state <- getStorage CMS.get
      let (Just v) = M.lookup loc state
      return v
    (_, (Just f)) -> do
      return $ funDefToVType f

declareFunction :: Ident -> InterpretMonad Env
declareFunction ident = do
  env <- ask
  return env { vEnv = M.delete ident (vEnv env) }
