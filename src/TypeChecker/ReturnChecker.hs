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
import           Common.Types

returnCheckExprM :: TCExpr -> TypeCheckerMonad ()
returnCheckExprM expr = putStmt (SExp (getExprContext expr) expr) $ go expr
 where
  go (ELambda ctx args ret block) = do
    returnCheckStmtM (SFnDef ctx ret (Ident "[]") args block)
    pure ()
  go (EApp _ _ exprs) = sequence_ $ fmap returnCheckExprM exprs
  go _                = pure ()

returnCheckStmtM :: TCStmt -> TypeCheckerMonad Bool
returnCheckStmtM stmt_ = putStmt stmt_ $ go stmt_
 where
  go (Ret _ _) = return True
  go (VRet _ ) = return True
  go (CondElse _ _ stmt1 stmt2) =
    liftM2 (&&) (returnCheckStmtM stmt1) (returnCheckStmtM stmt2)
  go (SFnDef ctx _ name _ block) = do
    blockCheck <- returnCheckStmtM (BStmt ctx block)
    ctx        <- asks stmt
    if not blockCheck then throwError $ ReturnMissing name ctx else return False
  go (BStmt _ (Block _ b)) = do
    checked <- sequence $ fmap returnCheckStmtM b
    return $ or checked
  go (Decl ctx t ((Init _ _ expr) : xs)) =
    returnCheckExprM expr >> returnCheckStmtM (Decl ctx t xs) >> return False
  go (Ass _ _ expr) = returnCheckExprM expr >> return False
  go _              = return False


returnCheckStmtsM :: [TCStmt] -> TypeCheckerMonad ()
returnCheckStmtsM stmts = sequence_ $ fmap returnCheckStmtM stmts
