{-# LANGUAGE FlexibleContexts #-}

module ValueTypes where

import           Samoyeet.Abs
import           Control.Monad
import           RuntimeError
import           Control.Monad.Except


data VType = VInt Integer | VStr String | VBool Bool | VVoid | VNone

instance Show VType where
  show VVoid       = "void"
  show VNone       = undefined
  show (VInt  val) = show val
  show (VStr  val) = show val
  show (VBool val) = show val

instance Eq VType where
  (VInt  left) == (VInt  right) = left == right
  (VStr  left) == (VStr  right) = left == right
  (VBool left) == (VBool right) = left == right


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

myAdd :: (MonadError RuntimeError m) => AddOp -> m VType -> m VType -> m VType
myAdd op left right = do
  l <- left
  r <- right
  (matcher op) l r
 where
  myPlus (VInt left) (VInt right) = pure . VInt $ left + right
  myPlus (VStr left) (VStr right) = pure . VStr $ left ++ right
  myMinus (VInt left) (VInt right) = pure . VInt $ left - right

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
