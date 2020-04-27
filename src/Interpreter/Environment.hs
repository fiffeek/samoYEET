module Interpreter.Environment where

import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Control.Monad.State           as CMS
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Interpreter.Types
import           Interpreter.RuntimeError

type InterpretMonad a
  = ReaderT Env (CMS.StateT Store (ExceptT RuntimeError IO)) a


putStmt :: Stmt -> InterpretMonad a -> InterpretMonad a
putStmt stmt_ cont = local (\e -> e { context = stmt_ }) cont

initialEnvironment :: Env
initialEnvironment =
  Env { vEnv = M.empty, pEnv = M.empty, vtype = VNone, context = Empty }

initialState :: Store
initialState = Store { storage = M.empty, freeAddresses = [1 ..] }

getStorage :: InterpretMonad Store -> InterpretMonad (M.Map MemAdr VType)
getStorage = fmap storage

getFreeAddr :: Store -> (MemAdr, [MemAdr])
getFreeAddr store = (head freeAddrs, tail freeAddrs)
  where freeAddrs = freeAddresses store

putValInit :: Ident -> VType -> InterpretMonad Env
putValInit name val = do
  env   <- ask
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  CMS.modify (putValInStore val splitState)
  return $ putVal name loc env
 where
  putValInStore :: VType -> (MemAdr, [MemAdr]) -> Store -> Store
  putValInStore val (freeAddr, rest) s =
    Store { storage = M.insert freeAddr val (storage s), freeAddresses = rest }

putValInAddr :: VType -> MemAdr -> Store -> Store
putValInAddr val addr s = Store { storage       = M.insert addr val (storage s)
                                , freeAddresses = freeAddresses s
                                }

putValNoInit :: Ident -> InterpretMonad Env
putValNoInit name = do
  env   <- ask
  state <- CMS.get
  let (loc, rest) = getFreeAddr state
  CMS.modify (updateFreeAddrs rest)
  return $ putVal name loc env
 where
  updateFreeAddrs :: [MemAdr] -> Store -> Store
  updateFreeAddrs rest s = Store { storage = storage s, freeAddresses = rest }

putVal :: Ident -> MemAdr -> Env -> Env
putVal name addr env =
  env { vEnv = M.insert name addr (vEnv env), pEnv = M.delete name (pEnv env) }

changeRetType :: VType -> Env -> Env
changeRetType v env = env { vEnv = vEnv env, pEnv = pEnv env, vtype = v }

flushVariables :: Env -> Env -> Env
flushVariables env1 env2 = changeRetType (vtype env2) env1

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
    , env   = Env { vEnv    = M.delete n (vEnv env)
                  , pEnv    = M.insert n makeFunc (pEnv env)
                  , vtype   = vtype env
                  , context = context env
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

getById :: Ident -> InterpretMonad VType
getById ident = do
  env <- ask
  let var = M.lookup ident (vEnv env)
  let f   = M.lookup ident (pEnv env)
  ctx <- asks context
  case (var, f) of
    ((Just loc), _) -> do
      state <- getStorage CMS.get
      v <- getOrError (M.lookup loc state) (VariableNotInitialized ident ctx)
      return v
    (_, (Just f)) -> do
      return $ funDefToVType f
    _ -> throwError (VariableNotInitialized ident ctx)

getOrError :: Maybe a -> RuntimeError -> InterpretMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

shadowName :: Ident -> Env -> Env
shadowName ident env = env { vEnv = M.delete ident (vEnv env) }
