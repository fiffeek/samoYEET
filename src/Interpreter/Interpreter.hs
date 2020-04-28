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

evaluateExprM :: IExpr -> InterpretMonad VType
evaluateExprM (ELitInt _ val           ) = return (VInt val)
evaluateExprM (ELitTrue  _             ) = return (VBool True)
evaluateExprM (ELitFalse _             ) = return (VBool False)
evaluateExprM (EString _ val           ) = return (VStr val)
evaluateExprM (ELambda _ args ret block) = do
  env <- ask
  return $ VFun args block env ret
evaluateExprM (Neg _ expr) = do
  fmap myNeg (evaluateExprM expr)
evaluateExprM (Not _ expr) = do
  fmap myNot (evaluateExprM expr)
evaluateExprM (EMul _ left op right) =
  myMul op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAdd _ left op right) =
  myAdd op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (ERel _ left op right) =
  myRel op (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EAnd _ left right) =
  liftM2 myAnd (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EOr _ left right) =
  liftM2 myOr (evaluateExprM left) (evaluateExprM right)
evaluateExprM (EVar _ var       ) = getById var
evaluateExprM (EApp _ name exprs) = do
  pEnv    <- asks pEnv
  currEnv <- ask
  ctx     <- asks context
  (FunctionDefinition _ _ args b@(Block _ stmts) envF) <- getOrError
    (M.lookup name pEnv)
    (VariableNotInitialized name ctx)
  let argExpr = args `zip` exprs
  envAfterVDecl <- local (const envF) $ dispatcher argExpr currEnv
  executeBlock  <- local (const envAfterVDecl) $ execStatementsM stmts
  when (vtype executeBlock == VNone) $ throwError ValueNotReturned
  return (vtype executeBlock)
 where
  dispatcher :: [(IArg, IExpr)] -> Env -> InterpretMonad Env
  dispatcher []                            _       = ask
  dispatcher (((Arg _ _ name), expr) : xs) evalEnv = do
    val    <- local (const evalEnv) $ evaluateExprM expr
    curEnv <- ask
    case val of
      (VFun args body env ret) -> do
        let envF = declareFunWithEnv ret name args body env curEnv
        local (const envF) $ dispatcher xs evalEnv
      _ -> do
        env <- putValInit name val
        local (const env) $ dispatcher xs evalEnv
  dispatcher (((RefArg _ _ name), (EVar _ calledWith)) : xs) evalEnv = do
    ctx <- asks context
    loc <- getOrError (M.lookup calledWith $ vEnv evalEnv)
                      (VariableNotInitialized calledWith ctx)
    local (putVal name loc) $ dispatcher xs evalEnv

execStatementM :: IStmt -> InterpretMonad Env
execStatementM stmt = putStmt stmt $ go stmt
 where
  go (Ret _ expr) = liftM2 changeRetType (evaluateExprM expr) ask

  go (BStmt _ (Block _ stmts)) =
    liftM2 flushVariables ask (execStatementsM stmts)

  go (Empty     _) = ask
  go (VRet      _) = ask >>= return . changeRetType VVoid
  go (SBreak    _) = ask >>= return . changeRetType VBreak
  go (SContinue _) = ask >>= return . changeRetType VContinue
  go (Decl _ _ []) = ask

  go (Decl ctx t@(Fun _ _ _) ((NoInit _ name) : xs)) =
    local (shadowName name) $ execStatementM (Decl ctx t xs)

  go (Decl ctx t ((NoInit _ name) : xs)) = do
    env <- putValNoInit name
    local (const env) $ execStatementM (Decl ctx t xs)

  go (Decl ctx t@(Fun _ _ _) ((Init _ name expr) : xs)) = do
    (VFun args body env ret) <- evaluateExprM expr
    local (declareFunWithEnv ret name args body env)
      $ execStatementM (Decl ctx t xs)

  go (Decl ctx t ((Init _ name expr) : xs)) = do
    val <- evaluateExprM expr
    env <- putValInit name val
    local (const env) $ execStatementM (Decl ctx t xs)

  go (Print _ expr   ) = evaluateExprM expr >>= liftIO . putStrLn . show >> ask

  go (Ass _ name expr) = do
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

  go (Incr _ name                ) = addToVar name 1
  go (Decr _ name                ) = addToVar name (-1)
  go (SExp _ expr                ) = evaluateExprM expr >> ask
  go (Cond ctx expr stmt) = execStatementM (CondElse ctx expr stmt (Empty ctx))

  go (CondElse _ expr stmt1 stmt2) = do
    val <- evaluateExprM expr
    liftM2
      flushVariables
      ask
      (if val == (VBool True)
        then execStatementM stmt1
        else execStatementM stmt2
      )

  go loop@(While _ expr stmt) = do
    val <- evaluateExprM expr
    if val == (VBool True)
      then execStatementM stmt >>= flip matchReturnType (execStatementM loop)
      else ask

  go (SFnDef _ retType name args block) = do
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

execStatementsM :: [IStmt] -> InterpretMonad Env
execStatementsM []       = ask
execStatementsM (x : []) = execStatementM x
execStatementsM (x : xs) = execStatementM x
  >>= flip statementValueDispatcher (execStatementsM xs)
 where
  statementValueDispatcher :: Env -> InterpretMonad Env -> InterpretMonad Env
  statementValueDispatcher env@(Env _ _ VNone _) stmts =
    local (const env) stmts
  statementValueDispatcher env _ = return env
