module Interpreter.Interpreter
  ( execStatementsM
  )
where
import           Interpreter.Environment
import           Control.Monad.State           as CMS
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Samoyeet.Abs
import           Interpreter.Types
import qualified Data.Map                      as M
import           Interpreter.RuntimeError

evaluateExprM :: Expr -> InterpretMonad VType
evaluateExprM (ELitInt val           ) = return (VInt val)
evaluateExprM (ELitTrue              ) = return (VBool True)
evaluateExprM (ELitFalse             ) = return (VBool False)
evaluateExprM (EString val           ) = return (VStr val)
evaluateExprM (ELambda args ret block) = do
  env <- ask
  return $ VFun args block env ret
evaluateExprM (Neg expr) = do
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
evaluateExprM (EVar var       ) = getById var
evaluateExprM (EApp name exprs) = do
  pEnv    <- asks pEnv
  currEnv <- ask
  ctx     <- asks context
  (FunctionDefinition _ _ args b@(Block stmts) envF) <- getOrError
    (M.lookup name pEnv)
    (VariableNotInitialized name ctx)
  let argExpr = args `zip` exprs
  envAfterVDecl <- local (const envF) $ dispatcher argExpr currEnv
  executeBlock  <- local (const envAfterVDecl) $ execStatementsM stmts
  when (vtype executeBlock == VNone) $ throwError ValueNotReturned
  return (vtype executeBlock)
 where
  dispatcher :: [(Arg, Expr)] -> Env -> InterpretMonad Env
  dispatcher []                          _       = ask
  dispatcher (((Arg _ name), expr) : xs) evalEnv = do
    val    <- local (const evalEnv) $ evaluateExprM expr
    curEnv <- ask
    case val of
      (VFun args body env ret) -> do
        let envF = declareFunWithEnv ret name args body env curEnv
        local (const envF) $ dispatcher xs evalEnv
      _ -> do
        env <- putValInit name val
        local (const env) $ dispatcher xs evalEnv
  dispatcher (((RefArg _ name), (EVar calledWith)) : xs) evalEnv = do
    ctx <- asks context
    loc <- getOrError (M.lookup calledWith $ vEnv evalEnv)
                      (VariableNotInitialized calledWith ctx)
    local (putVal name loc) $ dispatcher xs evalEnv

execStatementM :: Stmt -> InterpretMonad Env
execStatementM stmt = putStmt stmt $ go stmt
 where
  go (Ret   expr         ) = liftM2 changeRetType (evaluateExprM expr) ask

  go (BStmt (Block stmts)) = liftM2 flushVariables ask (execStatementsM stmts)

  go (Empty              ) = ask
  go (VRet               ) = ask >>= return . changeRetType VVoid
  go (SBreak             ) = ask >>= return . changeRetType VBreak
  go (SContinue          ) = ask >>= return . changeRetType VContinue
  go (Decl _ []          ) = ask

  go (Decl t@(Fun _ _) ((NoInit name) : xs)) =
    local (shadowName name) $ execStatementM (Decl t xs)

  go (Decl t ((NoInit name) : xs)) = do
    env <- putValNoInit name
    local (const env) $ execStatementM (Decl t xs)

  go (Decl t@(Fun _ _) ((Init name expr) : xs)) = do
    (VFun args body env ret) <- evaluateExprM expr
    local (declareFunWithEnv ret name args body env)
      $ execStatementM (Decl t xs)

  go (Decl t ((Init name expr) : xs)) = do
    val <- evaluateExprM expr
    env <- putValInit name val
    local (const env) $ execStatementM (Decl t xs)

  go (Print expr   ) = evaluateExprM expr >>= liftIO . putStrLn . show >> ask

  go (Ass name expr) = do
    val    <- evaluateExprM expr
    curEnv <- ask
    ctx    <- asks context
    case val of
      (VFun args body env ret) ->
        return $ declareFunWithEnv ret name args body env curEnv
      _ -> do
        env <- asks vEnv
        loc <- getOrError (M.lookup name env) (VariableNotInitialized name ctx)
        CMS.modify (putValInAddr val loc)
        return $ curEnv

  go (Incr name                ) = addToVar name 1
  go (Decr name                ) = addToVar name (-1)
  go (SExp expr                ) = evaluateExprM expr >> ask
  go (Cond expr stmt           ) = execStatementM (CondElse expr stmt Empty)

  go (CondElse expr stmt1 stmt2) = do
    val <- evaluateExprM expr
    liftM2
      flushVariables
      ask
      (if val == (VBool True)
        then execStatementM stmt1
        else execStatementM stmt2
      )

  go loop@(While expr stmt) = do
    val <- evaluateExprM expr
    if val == (VBool True)
      then execStatementM stmt >>= flip matchReturnType (execStatementM loop)
      else ask

  go (SFnDef retType name args block) = do
    env <- ask
    return (putFunInEnv retType name args block env)

  matchReturnType :: Env -> InterpretMonad Env -> InterpretMonad Env
  matchReturnType env@(Env _ _ VNone _) stmt = local (const env) stmt
  matchReturnType (Env _ _ VBreak _) _ = local (changeRetType VNone) ask
  matchReturnType (Env _ _ VContinue _) stmt = local (changeRetType VNone) stmt
  matchReturnType (Env _ _ t _) _ = local (changeRetType t) ask


addToVar :: Ident -> Integer -> InterpretMonad Env
addToVar name toAdd = do
  env   <- asks vEnv
  ctx   <- asks context
  loc   <- getOrError (M.lookup name env) (VariableNotInitialized name ctx)
  state <- CMS.get
  val   <- getOrError (M.lookup loc (storage state))
                      (VariableNotInitialized name ctx)
  let inc = myPlus val (VInt toAdd)
  CMS.modify (putValInAddr inc loc)
  ask

execStatementsM :: [Stmt] -> InterpretMonad Env
execStatementsM []       = ask
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = execStatementM x
  >>= flip statementValueDispatcher (execStatementsM xs)
 where
  statementValueDispatcher :: Env -> InterpretMonad Env -> InterpretMonad Env
  statementValueDispatcher env@(Env _ _ VNone _) stmts =
    local (const env) stmts
  statementValueDispatcher env _ = return env
