module Expressions where
import           Environments
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Utils
import           ValueTypes
import qualified Data.Map                      as M
import           RuntimeError


getOrError :: Maybe a -> RuntimeError -> InterpretMonad a
getOrError maybeVal error = maybe (throwError error) return maybeVal

evaluateExprM :: Expr -> InterpretMonad VType
evaluateExprM (ELitInt val ) = return (VInt val)
evaluateExprM (ELitTrue    ) = return (VBool True)
evaluateExprM (ELitFalse   ) = return (VBool False)
evaluateExprM (EString val ) = return (VStr val)
evaluateExprM (Neg     expr) = do
  fmap myNeg (evaluateExprM expr)
evaluateExprM (Not expr) = do
  fmap myNot (evaluateExprM expr)
evaluateExprM (EMul left op right) =
  myMul op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAdd left op right) =
  myAdd op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (ERel left op right) =
  myRel op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAnd left right) =
  liftM2 myAnd (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EOr left right) =
  liftM2 myOr (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EApp ident [exs]) = undefined
evaluateExprM (EVar var        ) = do
  env   <- asks vEnv
  loc   <- getOrError (M.lookup var env) VariableNotInitialized
  state <- getStorage CMS.get
  getOrError (M.lookup loc state) VariableMissingInStore
