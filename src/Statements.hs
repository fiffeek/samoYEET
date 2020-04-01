module Statements where
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
import           Expressions

execStatementM :: Stmt -> InterpretMonad Env
execStatementM (Ret expr) = do
  liftM2 changeRetType (evaluateExprM expr) ask
execStatementM (VRet) = do
  env <- ask
  return $ changeRetType VVoid env
execStatementM (BStmt (Block stmts)) = do
  liftM2 flushVariables ask (execStatementsM stmts)
execStatementM (Empty ) = ask
execStatementM (SBreak) = do
  env <- ask
  return $ changeRetType VBreak env
execStatementM (SContinue) = do
  env <- ask
  return $ changeRetType VContinue env
execStatementM (Decl t []                  ) = ask
execStatementM (Decl t ((NoInit name) : xs)) = do
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  local (putValInEnv name loc) $ execStatementM (Decl t xs)
execStatementM (Decl t ((Init name expr) : xs)) = do
  val   <- evaluateExprM expr
  state <- CMS.get
  let splitState@(loc, _) = getFreeAddr state
  CMS.modify (putValInStore val splitState)
  local (putValInEnv name loc) $ execStatementM (Decl t xs)
execStatementM (Print expr) = do
  val <- evaluateExprM expr
  liftIO . putStrLn . show $ val
  ask
execStatementM (Ass name expr) = do
  val <- evaluateExprM expr
  env <- asks vEnv
  loc <- getOrError (M.lookup name env) VariableNotInitialized
  CMS.modify (putValInAddr val loc)
  ask
execStatementM (Incr name                ) = addToVar name 1
execStatementM (Decr name                ) = addToVar name (-1)
execStatementM (Cond expr stmt) = execStatementM (CondElse expr stmt Empty)
execStatementM (CondElse expr stmt1 stmt2) = do
  val <- evaluateExprM expr
  liftM2
    flushVariables
    ask
    (if val == (VBool True) then execStatementM stmt1 else execStatementM stmt2)


addToVar :: Ident -> Integer -> InterpretMonad Env
addToVar name toAdd = do
  env   <- asks vEnv
  loc   <- getOrError (M.lookup name env) VariableNotInitialized
  state <- CMS.get
  val   <- getOrError (M.lookup loc (storage state)) VariableNotInitialized
  let inc = myPlus val (VInt toAdd)
  CMS.modify (putValInAddr inc loc)
  ask

execStatementsM :: [Stmt] -> InterpretMonad Env
execStatementsM []       = ask
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = do
  env <- execStatementM x
  statementValueDispatcher env (execStatementsM xs)
 where
  statementValueDispatcher :: Env -> InterpretMonad Env -> InterpretMonad Env
  statementValueDispatcher env@(Env _ _ VNone) stmts = local (const env) stmts
  statementValueDispatcher env                 _     = return env
