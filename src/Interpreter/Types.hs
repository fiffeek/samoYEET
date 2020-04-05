{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Types where

import qualified Data.Map                      as M
import           Samoyeet.Abs
import           Control.Monad
import           Interpreter.RuntimeError
import           Control.Monad.Except

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

data VType = VInt Integer
  | VStr String
  | VBool Bool
  | VVoid
  | VNone
  | VBreak
  | VContinue
  | VFun { fArgs:: [Arg], fBody :: Block, fEnv :: Env, fRet :: SType }

funDefToVType :: FunctionDefinition -> VType
funDefToVType (FunctionDefinition ret _ args body env) =
  VFun { fArgs = args, fBody = body, fEnv = env, fRet = ret }

instance Show VType where
  show (VInt  val) = show val
  show (VStr  val) = filter (/= '"') $ show val
  show (VBool val) = show val

instance Eq VType where
  (VInt  left) == (VInt  right) = left == right
  (VStr  left) == (VStr  right) = left == right
  (VBool left) == (VBool right) = left == right
  (VNone     ) == (VNone      ) = True
  _            == _             = False


instance Ord VType where
  (VInt  left) `compare` (VInt  right) = left `compare` right
  (VStr  left) `compare` (VStr  right) = left `compare` right
  (VBool left) `compare` (VBool right) = left `compare` right

myAnd (VBool left) (VBool right) = VBool $ and [left, right]
myOr (VBool left) (VBool right) = VBool $ or [left, right]
myNeg (VInt val) = VInt $ (-1) * val
myNot (VBool val) = VBool $ not val

myRel :: (MonadError RuntimeError m) => RelOp -> m VType -> m VType -> m VType
myRel op left right = do
  l <- left
  r <- right
  return . VBool $ (matcher op) l r
 where
  matcher :: RelOp -> VType -> VType -> Bool
  matcher LTH l r = l < r
  matcher LE  l r = l <= r
  matcher GTH l r = l > r
  matcher GE  l r = l >= r
  matcher EQU l r = l == r
  matcher NE  l r = l /= r

myPlus (VInt left) (VInt right) = VInt $ left + right
myPlus (VStr left) (VStr right) = VStr $ left ++ right
myMinus (VInt left) (VInt right) = VInt $ left - right

myAdd :: (MonadError RuntimeError m) => AddOp -> m VType -> m VType -> m VType
myAdd op left right = do
  l <- left
  r <- right
  pure $ (matcher op) l r
 where
  matcher Plus  = myPlus
  matcher Minus = myMinus

myMul :: (MonadError RuntimeError m) => MulOp -> m VType -> m VType -> m VType
myMul op left right = do
  l <- left
  r <- right
  (matcher op) l r
 where
  myTimes (VInt left) (VInt right) = pure . VInt $ left * right

  myDiv (VInt left) (VInt 0    ) = throwError DivisionByZero
  myDiv (VInt left) (VInt right) = pure . VInt $ left `div` right

  myMod (VInt left) (VInt right) = pure . VInt $ left `mod` right

  matcher Times = myTimes
  matcher Div   = myDiv
  matcher Mod   = myMod
