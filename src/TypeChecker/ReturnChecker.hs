module TypeChecker.ReturnChecker where
import           Samoyeet.Abs
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import qualified Data.Map                      as M
import           Control.Applicative
import           TypeChecker.Converter
import           TypeChecker.Environment
import           TypeChecker.TypeError
import           TypeChecker.Types

returnCheckExprM :: TCExpr -> TypeCheckerMonad ()
returnCheckExprM (ELambda ctx args ret block) = do
  returnCheckStmtM (SFnDef ctx ret (Ident "[]") args block)
  pure ()
returnCheckExprM (EApp _ _ exprs) = sequence_ $ fmap returnCheckExprM exprs
returnCheckExprM _                = pure ()

returnCheckStmtM :: TCStmt -> TypeCheckerMonad Bool
returnCheckStmtM (Ret _ _) = return True
returnCheckStmtM (VRet _ ) = return True
returnCheckStmtM (CondElse _ _ stmt1 stmt2) =
  liftM2 (&&) (returnCheckStmtM stmt1) (returnCheckStmtM stmt2)
returnCheckStmtM (SFnDef ctx _ name _ block) = do
  blockCheck <- returnCheckStmtM (BStmt ctx block)
  if not blockCheck then throwError $ ReturnMissing name else return False
returnCheckStmtM (BStmt _ (Block _ b)) = do
  checked <- sequence $ fmap returnCheckStmtM b
  return $ or checked
returnCheckStmtM (Decl ctx t ((Init _ _ expr) : xs)) =
  returnCheckExprM expr >> returnCheckStmtM (Decl ctx t xs) >> return False
returnCheckStmtM (Ass _ _ expr) = returnCheckExprM expr >> return False
returnCheckStmtM _              = return False


returnCheckStmtsM :: [TCStmt] -> TypeCheckerMonad ()
returnCheckStmtsM stmts = sequence_ $ fmap returnCheckStmtM stmts
