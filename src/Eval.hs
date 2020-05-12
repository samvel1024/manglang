module Eval where

import           Data.Map               (Map)
import qualified Data.Map               as Map

import           AbsDeclaration
import           Control.Arrow
import           Control.Monad.Except
import           Data.Foldable

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

data Value
  = VBool Bool
  | VNum Integer
  | VChar Char
  | VFun TopDef Env
  | VVoid
  | VBreak
  | VContinue
  deriving (Eq)

instance Show Value where
  show x =
    case x of
      VBreak    -> "break"
      VContinue -> "continue"
      VVoid     -> "void"
      VBool b   -> "Bool(" ++ show b ++ ")"
      VNum n    -> "Num(" ++ show n ++ ")"
      VFun f _  -> "Fun(" ++ show f ++ ")"

type Loc = Integer

type Env = Map Ident Loc

type Mem = Map Loc Value

type ThreadCtx = (Mem, Loc)

type Res = ReaderT Env (StateT ThreadCtx (ExceptT String IO))

type ResF a = Res a -> Res a

interpret :: Program -> IO ()
interpret p = do
  ans <- runExceptT (runStateT (runReaderT (interpret0 p) Map.empty) (Map.empty, 0))
  case ans of
    (Left e) -> putStrLn $ "Error: " ++ e
    _        -> return ()

interpret0 :: Program -> Res ()
interpret0 (Program defs) = foldr (acc . declare0) configure defs
  where
    acc :: Res (Res () -> Res ()) -> Res () -> Res ()
    acc env0 res0 = env0 >>= (\env -> env res0)
    declare0 :: TopDef -> Res (ResF ())
    declare0 def = do
      env <- ask
      case def of
        FnDef rt ident args block -> declare ident (return $ VFun def env)
    configure :: Res ()
    configure = do
      genv <- ask
      (mem, loc) <- get
      traverse_
        (\(k, v) ->
           case v of
             (VFun fun env) -> writeMem (Map.insert k (VFun fun genv))
             _              -> writeMem id)
        (Map.toList mem)
      case Map.lookup (Ident "main") genv of
        Just mLoc -> do
          VFun mainFun _  <- findLoc mLoc
          evalFun mainFun genv []
          return ()
        Nothing -> throwError "Main function is not defined"

findLoc :: Loc -> Res Value
findLoc l = do
  mem <- gets fst
  case Map.lookup l mem of
    Just v  -> return v
    Nothing -> throwError $ "Undefined loc " ++ show l

findIdent :: Ident -> Res Value
findIdent id = do
  env <- ask
  let loc0 = Map.lookup id env
  case loc0 of
    Just loc -> findLoc loc
    Nothing  -> throwError $ "Undefined identifier" ++ show id

declare :: Ident -> Res Value -> Res (ResF a)
declare ident rval = do
  val <- rval
  loc <- newloc
  let thisFunc =
        case val of
          VFun f env -> VFun f (Map.insert ident loc env)
          _          -> val
  writeMem (Map.insert loc thisFunc)
  return $ local (Map.insert ident loc)

declareMany :: [(Ident, Res Value)] -> Res (ResF a)
declareMany ((id, f):xs) = do
  g <- declare id f
  next <- declareMany xs
  return $ g . next
declareMany [] = return (local id)

writeMem :: (Mem -> Mem) -> Res ()
writeMem f = modify (first f)

newloc :: Res Loc
newloc = do
  (mem, lloc) <- get
  put (mem, lloc + 1)
  return lloc

debug :: String -> Res ()
debug str = liftIO $ putStrLn str

evalFun :: TopDef -> Env -> [Res Value] -> Res Value
evalFun (FnDef ftype name args block) env argVals = do
  declFun <- declareMany $ zip (map (\(Arg _ nm) -> nm) args) argVals
  let Block sts = block
  result <- local (const env) (declFun (evalBlock sts))
  case result of
    Nothing  -> return VVoid
    Just val -> return val

evalBlock :: [Stmt] -> Res (Maybe Value)
evalBlock (h:tl) =
  case h of
    Empty -> evalBlock tl
    BStmt (Block stmts) -> do
      blockRes <- local id (evalBlock stmts)
      case blockRes of
        Nothing       -> evalBlock tl
        Just finalVal -> return (Just finalVal)
    Decl type_ items -> do
      let initValues = map (declareValue type_) items
      declCont <- declareMany $ map getIdent items `zip` initValues
      declCont (evalBlock tl)
    DeclFinal type_ items -> evalBlock (Decl type_ items : tl)
    Ass ident expr -> evalExpr expr >>= (assignVar ident . const) >> evalBlock tl
    Incr ident -> evalBlock $ Ass ident (EAdd (EVar ident) Plus (ELitInt 1)) : tl
    Decr ident -> evalBlock $ Ass ident (EAdd (EVar ident) Minus (ELitInt 1)) : tl
    ConstFor type_ ident expr1 expr2 stmt -> do
      (VNum start) <- evalExpr expr1
      (VNum end) <- evalExpr expr2
      loc <- newloc
      local (Map.insert ident loc) (evalFor start loc end stmt) >> evalBlock tl
    Ret expr -> Just <$> evalExpr expr
    VRet -> return (Just VVoid)
    Print expr -> evalExpr expr >>= (liftIO . print) >> evalBlock tl
    Cond expr stmt -> evalBlock $ CondElse expr stmt Empty : tl
    CondElse expr stmt1 stmt2 ->
      let evalAsBlock stmt = evalBlock $ BStmt (Block [stmt]) : tl
       in evalExpr expr >>=
          (\(VBool cond) ->
             if cond
               then evalAsBlock stmt1
               else evalAsBlock stmt2)
    While expr stmt -> do
      res <- evalWhile expr stmt
      case res of
        Nothing -> evalBlock tl
        Just _  -> return res
    Break -> return $ Just VBreak
    Continue -> return $ Just VContinue
    SExp expr -> evalExpr expr >> evalBlock tl
  where
    declareValue :: Type -> Item -> Res Value
    declareValue tp (NoInit id) = defaultPerType tp
    declareValue tp (Init id e) = evalExpr e
    defaultPerType :: Type -> Res Value
    defaultPerType t =
      case t of
        Int  -> return $ VNum 0
        Char -> return $ VChar '\0'
        Void -> return VVoid
        Bool -> return $ VBool False
    getIdent (NoInit id) = id
    getIdent (Init id _) = id
    assignVar :: Ident -> (Value -> Value) -> Res ()
    assignVar id@(Ident name) f = do
      env <- ask
      let loc0 = Map.lookup id env
      case loc0 of
        Nothing -> throwError $ "Unkown identifier " ++ name
        Just l  -> findLoc l >>= (writeMem . Map.insert l . f)
    evalWhile :: Expr -> Stmt -> Res (Maybe Value)
    evalWhile expr stmt = do
      VBool next <- evalExpr expr
      if next
        then do
          ans <- evalBlock [BStmt $ Block [stmt]]
          case ans of
            Nothing        -> evalWhile expr stmt
            Just VBreak    -> return Nothing
            Just VContinue -> evalWhile expr stmt
            Just _         -> return ans
        else return Nothing
    evalFor :: Integer -> Integer -> Loc -> Stmt -> Res ()
    evalFor curr end loc stmt =
      when (curr <= end) (writeMem (Map.insert loc (VNum curr))) >> evalBlock [BStmt $ Block [stmt]] >>
      evalFor (curr + 1) end loc stmt
evalBlock [] = return Nothing

evalExpr :: Expr -> Res Value
evalExpr expr =
  case expr of
    ELitInt e -> return $ VNum e
    ELitTrue -> return $ VBool True
    ELitFalse -> return $ VBool False
    Neg e -> evalExpr e >>= \(VNum v) -> return $ VNum (-v)
    Not e -> evalExpr e >>= \(VBool v) -> return $ VBool $not v
    EAdd e1 op e2 -> do
      v1 <- evalExpr e1
      v2 <- evalExpr e2
      case (v1, v2, op) of
        (VNum v1, VNum v2, Plus) -> return $ VNum $ v1 + v2
        (VNum v1, VNum v2, Minus) -> return $ VNum $ v1 - v2
        _ -> throwError $ "Cannot add " ++ show v1 ++ " to " ++ show v2
    ERel e1 op e2 -> do
      v1 <- evalExpr e1
      v2 <- evalExpr e2
      VBool <$>
        (case op of
           LTH -> lth v1 v2
           GTH -> lth v2 v1
           LE  -> lth v1 v2 `or` equ v1 v2
           GE  -> lth v2 v1 `or` equ v1 v2
           EQU -> equ v1 v2
           NE  -> not <$> equ v1 v2)
    EMul e1 op e2 -> do
      v1 <- evalExpr e1
      v2 <- evalExpr e2
      case (v1, v2, op) of
        (VNum v1, VNum 0, Div)    -> throwError "Division by zero"
        (VNum v1, VNum 0, Mod)    -> throwError "Division by zero"
        (VNum v1, VNum v2, Times) -> return $ VNum (v1 * v2)
        (VNum v1, VNum v2, Mod)   -> return $ VNum (v1 `mod` v2)
        (VNum v1, VNum v2, Div)   -> return $ VNum (v1 `quot` v2)
    EAnd e1 e2 -> do
      v1 <- evalExpr e1
      case v1 of
        VBool False -> return v1
        VBool True  -> evalExpr e2
    EOr e1 e2 -> do
      v1 <- evalExpr e1
      case v1 of
        VBool True  -> return v1
        VBool False -> evalExpr e2
    EApp ident args -> do
      let argVals = map evalExpr args
      (VFun f e) <- findIdent ident
      evalFun f e argVals
  where
    lth, equ :: Value -> Value -> Res Bool
    lth (VBool x1) (VBool x2) = return $ x1 < x2
    lth (VNum x1) (VNum x2) = return $ x1 < x2
    lth x1 x2 = throwError $ "Cannot compare " ++ show x1 ++ " and " ++ show x2
    equ (VBool x1) (VBool x2) = return $ x1 == x2
    equ (VNum x1) (VNum x2) = return $ x1 == x2
    equ x1 x2 = throwError $ "Cannot compare " ++ show x1 ++ "and " ++ show x2
    or x1 x2 = (||) <$> x1 <*> x2
