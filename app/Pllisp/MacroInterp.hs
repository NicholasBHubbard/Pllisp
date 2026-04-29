{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.MacroInterp
  ( MVal(..)
  , Env
  , InterpM
  , eval
  , evalTyped
  , runInterpM
  , defaultEnv
  , loadTopLevelForm
  , loadTopLevelForms
  , loadTypedTopLevelForms
  , sexprToVal
  , valToSExpr
  ) where

import qualified Pllisp.CST    as CST
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Control.Monad.State.Strict as State

import qualified Pllisp.Resolve as Res
import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

-- CORE TYPES

data MVal
  = MAtom T.Text
  | MStr T.Text
  | MInt Integer
  | MFlt Double
  | MList [MVal]
  | MBool Bool
  | MRx T.Text T.Text
  | MUSym T.Text
  | MType MVal
  | MQuasi MVal
  | MUnquote MVal
  | MSplice MVal
  | MData T.Text [MVal]
  | MCtor T.Text Int [MVal]
  | MClosure Env [T.Text] SExpr.SExpr
  | MTypedClosure Env [T.Text] TC.TRExpr
  | MBuiltin T.Text ([MVal] -> InterpM MVal)

instance Show MVal where
  show (MAtom t)   = "MAtom " ++ show t
  show (MStr t)    = "MStr " ++ show t
  show (MInt n)    = "MInt " ++ show n
  show (MFlt f)    = "MFlt " ++ show f
  show (MList xs)  = "MList " ++ show xs
  show (MBool b)   = "MBool " ++ show b
  show (MRx p f)   = "MRx " ++ show p ++ " " ++ show f
  show (MUSym t)   = "MUSym " ++ show t
  show (MType v)   = "MType " ++ show v
  show (MQuasi v)  = "MQuasi " ++ show v
  show (MUnquote v) = "MUnquote " ++ show v
  show (MSplice v) = "MSplice " ++ show v
  show (MData n xs) = "MData " ++ show n ++ " " ++ show xs
  show (MCtor n _ xs) = "MCtor " ++ show n ++ " " ++ show xs
  show (MClosure _ ps _) = "MClosure <" ++ show ps ++ ">"
  show (MTypedClosure _ ps _) = "MTypedClosure <" ++ show ps ++ ">"
  show (MBuiltin n _)    = "MBuiltin " ++ show n

instance Eq MVal where
  MAtom a   == MAtom b   = a == b
  MStr a    == MStr b    = a == b
  MInt a    == MInt b    = a == b
  MFlt a    == MFlt b    = a == b
  MList a   == MList b   = a == b
  MBool a   == MBool b   = a == b
  MRx a b   == MRx c d   = a == c && b == d
  MUSym a   == MUSym b   = a == b
  MType a   == MType b   = a == b
  MQuasi a  == MQuasi b  = a == b
  MUnquote a == MUnquote b = a == b
  MSplice a == MSplice b = a == b
  MData n xs == MData m ys = n == m && xs == ys
  _         == _         = False

type Env = M.Map T.Text MVal

type InterpM = State.StateT Int (Either String)

throwError :: String -> InterpM a
throwError = State.StateT . const . Left

runInterpM :: InterpM a -> Either String a
runInterpM m = State.evalStateT m 0

-- CONVERSION: SExpr <-> MVal

sexprToVal :: SExpr.SExpr -> MVal
sexprToVal (Loc.Located _ sf) = case sf of
  SExpr.SAtom "TRUE"  -> MBool True
  SExpr.SAtom "FALSE" -> MBool False
  SExpr.SAtom t       -> MAtom t
  SExpr.SStr t        -> MStr t
  SExpr.SInt n        -> MInt n
  SExpr.SFlt f        -> MFlt f
  SExpr.SRx p f       -> MRx p f
  SExpr.SUSym t       -> MUSym t
  SExpr.SList xs      -> MList (map sexprToVal xs)
  SExpr.SType inner   -> MType (sexprToVal inner)
  SExpr.SQuasi inner  -> MQuasi (sexprToVal inner)
  SExpr.SUnquote inner -> MUnquote (sexprToVal inner)
  SExpr.SSplice inner -> MSplice (sexprToVal inner)

valToSExpr :: MVal -> Either String SExpr.SExpr
valToSExpr val = case val of
  MAtom t  -> Right $ loc $ SExpr.SAtom t
  MStr t   -> Right $ loc $ SExpr.SStr t
  MInt n   -> Right $ loc $ SExpr.SInt n
  MFlt f   -> Right $ loc $ SExpr.SFlt f
  MRx p f  -> Right $ loc $ SExpr.SRx p f
  MUSym t  -> Right $ loc $ SExpr.SUSym t
  MBool True  -> Right $ loc $ SExpr.SAtom "TRUE"
  MBool False -> Right $ loc $ SExpr.SAtom "FALSE"
  MList xs -> do
    xs' <- mapM valToSExpr xs
    Right $ loc $ SExpr.SList xs'
  MType inner -> do
    inner' <- valToSExpr inner
    Right $ loc $ SExpr.SType inner'
  MQuasi inner -> do
    inner' <- valToSExpr inner
    Right $ loc $ SExpr.SQuasi inner'
  MUnquote inner -> do
    inner' <- valToSExpr inner
    Right $ loc $ SExpr.SUnquote inner'
  MSplice inner -> do
    inner' <- valToSExpr inner
    Right $ loc $ SExpr.SSplice inner'
  MData {} -> Left "cannot convert data value to syntax"
  MCtor {} -> Left "cannot convert constructor to syntax"
  MClosure {} -> Left "cannot convert closure to syntax"
  MTypedClosure {} -> Left "cannot convert typed closure to syntax"
  MBuiltin {} -> Left "cannot convert builtin to syntax"
  where
    loc = Loc.Located dummySpan

-- EVALUATOR

eval :: Env -> SExpr.SExpr -> InterpM MVal
eval env (Loc.Located _ sf) = case sf of
  -- Self-evaluating literals
  SExpr.SInt n  -> pure $ MInt n
  SExpr.SFlt f  -> pure $ MFlt f
  SExpr.SStr t  -> pure $ MStr t
  SExpr.SRx p f -> pure $ MRx p f
  SExpr.SUSym t -> pure $ MUSym t

  -- Atoms: special values or variable lookup
  SExpr.SAtom "TRUE"  -> pure $ MBool True
  SExpr.SAtom "FALSE" -> pure $ MBool False
  SExpr.SAtom name    -> case M.lookup name env of
    Just val -> pure val
    Nothing  -> throwError $ "undefined variable: " ++ T.unpack name

  -- Type annotations: preserve as syntax
  SExpr.SType inner -> pure $ MType (sexprToVal inner)

  -- Quasiquote: construct value
  SExpr.SQuasi inner -> evalQuasi env inner

  -- Unquote/splice outside quasiquote
  SExpr.SUnquote _ -> throwError "unquote outside quasiquote"
  SExpr.SSplice _  -> throwError "splice outside quasiquote"

  -- Lists: special forms or application
  SExpr.SList [] -> pure $ MList []
  SExpr.SList (Loc.Located _ (SExpr.SAtom "QUOTE") : rest) -> evalQuote rest
  SExpr.SList (Loc.Located _ (SExpr.SAtom "LET") : rest)   -> evalLet env rest
  SExpr.SList (Loc.Located _ (SExpr.SAtom "LAM") : rest)    -> evalLam env rest
  SExpr.SList (Loc.Located _ (SExpr.SAtom "IF") : rest)     -> evalIf env rest
  SExpr.SList (Loc.Located _ (SExpr.SAtom "AND") : rest)    -> evalAnd env rest
  SExpr.SList (Loc.Located _ (SExpr.SAtom "OR") : rest)     -> evalOr env rest
  SExpr.SList (fn : args) -> do
    fnVal  <- eval env fn
    argVals <- mapM (eval env) args
    apply fnVal argVals

-- SPECIAL FORMS

evalQuote :: [SExpr.SExpr] -> InterpM MVal
evalQuote [sx] = pure $ sexprToVal sx
evalQuote _    = throwError "quote expects exactly one argument"

evalLet :: Env -> [SExpr.SExpr] -> InterpM MVal
evalLet env [Loc.Located _ (SExpr.SList bindings), body] = do
  env' <- bindSequential env bindings
  eval env' body
evalLet _ _ = throwError "invalid let: expected (let ((name val)...) body)"

bindSequential :: Env -> [SExpr.SExpr] -> InterpM Env
bindSequential env [] = pure env
bindSequential env (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom name), valExpr]) : rest) = do
  val <- eval env valExpr
  -- Make recursive lambdas work: patch closure env to include itself
  let val' = case val of
        MClosure cEnv params body ->
          let cEnv' = M.insert name val' cEnv
          in MClosure cEnv' params body
        other -> other
  bindSequential (M.insert name val' env) rest
bindSequential _ _ = throwError "invalid let binding: expected (name value)"

evalLam :: Env -> [SExpr.SExpr] -> InterpM MVal
evalLam env [Loc.Located _ (SExpr.SList params), body] = do
  paramNames <- mapM extractParamName params
  pure $ MClosure env paramNames body
-- With return type annotation: (lam (params) %type body)
evalLam env [Loc.Located _ (SExpr.SList params), Loc.Located _ (SExpr.SType _), body] = do
  paramNames <- mapM extractParamName params
  pure $ MClosure env paramNames body
evalLam _ _ = throwError "invalid lambda: expected (lam (params...) body)"

extractParamName :: SExpr.SExpr -> InterpM T.Text
extractParamName (Loc.Located _ (SExpr.SAtom name)) = pure name
-- Typed param: (name %type)
extractParamName (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom name) : _))) = pure name
extractParamName _ = throwError "invalid lambda parameter"

evalIf :: Env -> [SExpr.SExpr] -> InterpM MVal
evalIf env [cond, thenBr, elseBr] = do
  condVal <- eval env cond
  if truthy condVal
    then eval env thenBr
    else eval env elseBr
evalIf _ _ = throwError "invalid if: expected (if cond then else)"

evalAnd :: Env -> [SExpr.SExpr] -> InterpM MVal
evalAnd _ [] = pure $ MBool True
evalAnd env [x] = eval env x
evalAnd env (x : rest) = do
  v <- eval env x
  if truthy v then evalAnd env rest else pure $ MBool False

evalOr :: Env -> [SExpr.SExpr] -> InterpM MVal
evalOr _ [] = pure $ MBool False
evalOr env [x] = eval env x
evalOr env (x : rest) = do
  v <- eval env x
  if truthy v then pure v else evalOr env rest

-- QUASIQUOTE EVALUATION

evalQuasi :: Env -> SExpr.SExpr -> InterpM MVal
evalQuasi env (Loc.Located _ sf) = case sf of
  SExpr.SUnquote inner -> eval env inner
  SExpr.SList elems    -> MList <$> evalQuasiList env elems
  SExpr.SAtom "TRUE"   -> pure $ MBool True
  SExpr.SAtom "FALSE"  -> pure $ MBool False
  SExpr.SAtom t        -> pure $ MAtom t
  SExpr.SInt n         -> pure $ MInt n
  SExpr.SFlt f         -> pure $ MFlt f
  SExpr.SStr t         -> pure $ MStr t
  SExpr.SRx p f        -> pure $ MRx p f
  SExpr.SUSym t        -> pure $ MUSym t
  SExpr.SType inner    -> pure $ MType (sexprToVal inner)
  SExpr.SQuasi _       -> pure $ sexprToVal (Loc.Located dummySpan sf)
  SExpr.SSplice _      -> throwError "splice outside list in quasiquote"

evalQuasiList :: Env -> [SExpr.SExpr] -> InterpM [MVal]
evalQuasiList _ [] = pure []
evalQuasiList env (Loc.Located _ (SExpr.SSplice inner) : rest) = do
  val <- eval env inner
  case val of
    MList xs -> do
      rest' <- evalQuasiList env rest
      pure (xs ++ rest')
    _ -> throwError ",@ requires a list value"
evalQuasiList env (x : rest) = do
  v    <- evalQuasi env x
  rest' <- evalQuasiList env rest
  pure (v : rest')

-- TYPED EVALUATOR

evalTyped :: Env -> TC.TRExpr -> InterpM MVal
evalTyped env (Loc.Located _ (Ty.Typed _ expr)) = case expr of
  TC.TRLit lit -> pure (litToVal lit)
  TC.TRBool b  -> pure (MBool b)
  TC.TRUnit    -> pure (MAtom "UNIT")
  TC.TRVar vb  -> case M.lookup (Res.symName vb) env of
    Just val -> pure val
    Nothing  -> throwError $ "undefined variable: " ++ T.unpack (Res.symName vb)
  TC.TRLam params _ body ->
    pure $ MTypedClosure env (map fst params) body
  TC.TRLet binds body -> do
    env' <- bindTypedSequential env binds
    evalTyped env' body
  TC.TRIf cond thenBr elseBr -> do
    condVal <- evalTyped env cond
    if truthy condVal
      then evalTyped env thenBr
      else evalTyped env elseBr
  TC.TRApp fn args -> do
    fnVal <- evalTyped env fn
    argVals <- mapM (evalTyped env) args
    apply fnVal argVals
  TC.TRType _ _ _ -> pure (MAtom "UNIT")
  TC.TRCase scrutinee arms -> do
    scrutVal <- evalTyped env scrutinee
    evalTypedCase env scrutVal arms
  TC.TRLoop params body -> do
    let paramNames = map fst params
    initialVals <- mapM lookupLoopParam paramNames
    loop paramNames initialVals
      where
        lookupLoopParam name = case M.lookup name env of
          Just val -> pure val
          Nothing  -> throwError $ "undefined loop parameter: " ++ T.unpack name
        loop names vals = do
          let loopEnv = M.union (M.fromList (zip names vals)) env
          result <- evalTypedLoopBody loopEnv body
          case result of
            Left recurVals -> loop names recurVals
            Right val -> pure val
  TC.TRRecur _ ->
    throwError "recur outside loop"
  TC.TRFFI name _ _ ->
    pure $ ffiStub name
  TC.TRFFIStruct _ _ ->
    pure (MAtom "UNIT")
  TC.TRFFIVar name _ _ ->
    pure $ ffiStub name
  TC.TRFFIEnum _ _ ->
    pure (MAtom "UNIT")
  TC.TRFFICallback name _ _ ->
    pure $ ffiStub name

evalTypedLoopBody :: Env -> TC.TRExpr -> InterpM (Either [MVal] MVal)
evalTypedLoopBody env trExpr@(Loc.Located _ (Ty.Typed _ expr)) = case expr of
  TC.TRRecur args ->
    Left <$> mapM (evalTyped env) args
  TC.TRIf cond thenBr elseBr -> do
    condVal <- evalTyped env cond
    if truthy condVal
      then evalTypedLoopBody env thenBr
      else evalTypedLoopBody env elseBr
  TC.TRLet binds body -> do
    env' <- bindTypedSequential env binds
    evalTypedLoopBody env' body
  TC.TRCase scrutinee arms -> do
    scrutVal <- evalTyped env scrutinee
    evalTypedLoopCase env scrutVal arms
  _ -> Right <$> evalTyped env trExpr

evalTypedCase :: Env -> MVal -> [(TC.TRPattern, TC.TRExpr)] -> InterpM MVal
evalTypedCase _ _ [] = throwError "non-exhaustive case in compile-time evaluation"
evalTypedCase env scrutinee ((pat, body) : rest) =
  case matchPattern pat scrutinee of
    Just binds -> evalTyped (M.union (M.fromList binds) env) body
    Nothing -> evalTypedCase env scrutinee rest

evalTypedLoopCase :: Env -> MVal -> [(TC.TRPattern, TC.TRExpr)] -> InterpM (Either [MVal] MVal)
evalTypedLoopCase _ _ [] = throwError "non-exhaustive case in compile-time evaluation"
evalTypedLoopCase env scrutinee ((pat, body) : rest) =
  case matchPattern pat scrutinee of
    Just binds -> evalTypedLoopBody (M.union (M.fromList binds) env) body
    Nothing -> evalTypedLoopCase env scrutinee rest

matchPattern :: TC.TRPattern -> MVal -> Maybe [(T.Text, MVal)]
matchPattern pat val = case pat of
  TC.TRPatLit lit
    | litToVal lit == val -> Just []
    | otherwise -> Nothing
  TC.TRPatBool b
    | MBool b == val -> Just []
    | otherwise -> Nothing
  TC.TRPatVar name _ -> Just [(name, val)]
  TC.TRPatWild _ -> Just []
  TC.TRPatCon name _ subpats -> case val of
    MData ctorName fields
      | ctorName == name && length fields == length subpats ->
          fmap concat (sequence (zipWith matchPattern subpats fields))
      | otherwise -> Nothing
    _ -> Nothing

bindTypedSequential :: Env -> [(CST.Symbol, t, TC.TRExpr)] -> InterpM Env
bindTypedSequential env [] = pure env
bindTypedSequential env ((name, _, rhs) : rest) = do
  val <- evalTyped env rhs
  let val' = case val of
        MClosure cEnv params body ->
          let cEnv' = M.insert name val' cEnv
          in MClosure cEnv' params body
        MTypedClosure cEnv params body ->
          let cEnv' = M.insert name val' cEnv
          in MTypedClosure cEnv' params body
        other -> other
  bindTypedSequential (M.insert name val' env) rest

loadTypedTopLevelForms :: Env -> [TC.TRExpr] -> InterpM Env
loadTypedTopLevelForms env [] = pure env
loadTypedTopLevelForms env (expr : rest) = do
  env' <- loadTypedTopLevelForm env expr
  loadTypedTopLevelForms env' rest

loadTypedTopLevelForm :: Env -> TC.TRExpr -> InterpM Env
loadTypedTopLevelForm env trExpr@(Loc.Located _ (Ty.Typed _ expr)) = case expr of
  TC.TRType _ _ ctors ->
    pure $ foldl registerCtor env ctors
  TC.TRFFI name _ _ ->
    pure $ M.insert name (ffiStub name) env
  TC.TRFFIVar name _ _ ->
    pure $ M.insert name (ffiStub name) env
  TC.TRFFICallback name _ _ ->
    pure $ M.insert name (ffiStub name) env
  TC.TRFFIEnum _ variants ->
    pure $ foldl (\acc (name, n) -> M.insert name (MInt n) acc) env variants
  TC.TRLet binds body -> do
    env' <- bindTypedSequential env binds
    _ <- evalTyped env' body
    pure env'
  _ -> do
    _ <- evalTyped env trExpr
    pure env

registerCtor :: Env -> CST.DataCon -> Env
registerCtor env dc =
  M.insert (CST.dcName dc) ctor env
  where
    arity = length (CST.dcArgs dc)
    ctor
      | arity == 0 = MData (CST.dcName dc) []
      | otherwise = MCtor (CST.dcName dc) arity []

ffiStub :: T.Text -> MVal
ffiStub name = MBuiltin name (\_ -> throwError ("ffi not available at macro expansion time: " ++ T.unpack name))

litToVal :: CST.Literal -> MVal
litToVal lit = case lit of
  CST.LitInt n -> MInt n
  CST.LitFlt f -> MFlt f
  CST.LitStr t -> MStr t
  CST.LitRx p f -> MRx p f
  CST.LitUSym t -> MUSym t

-- FUNCTION APPLICATION

apply :: MVal -> [MVal] -> InterpM MVal
apply (MClosure closureEnv params body) args
  | length params == length args = do
      let env = M.union (M.fromList (zip params args)) closureEnv
      eval env body
  | otherwise = throwError $ "wrong number of arguments: expected "
      ++ show (length params) ++ ", got " ++ show (length args)
apply (MTypedClosure closureEnv params body) args
  | length params == length args = do
      let env = M.union (M.fromList (zip params args)) closureEnv
      evalTyped env body
  | otherwise = throwError $ "wrong number of arguments: expected "
      ++ show (length params) ++ ", got " ++ show (length args)
apply (MCtor name arity applied) args
  | length combined == arity = pure $ MData name combined
  | length combined < arity = pure $ MCtor name arity combined
  | otherwise = throwError $ "wrong number of arguments: expected "
      ++ show arity ++ ", got " ++ show (length combined)
  where
    combined = applied ++ args
apply (MBuiltin _ f) args = f args
apply val _ = throwError $ "not a function: " ++ showBrief val

-- TRUTHINESS

truthy :: MVal -> Bool
truthy (MBool b)   = b
truthy (MList [])  = False
truthy (MList _)   = True
truthy (MInt 0)    = False
truthy (MInt _)    = True
truthy (MStr "")   = False
truthy (MStr _)    = True
truthy _           = True

-- DEFAULT ENVIRONMENT

defaultEnv :: Env
defaultEnv = primitiveEnv

loadTopLevelForm :: Env -> SExpr.SExpr -> InterpM Env
loadTopLevelForm env (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom "LET"), Loc.Located _ (SExpr.SList binds), body])) = do
  env' <- bindSequential env binds
  _ <- eval env' body
  pure env'
loadTopLevelForm env sx = do
  _ <- eval env sx
  pure env

loadTopLevelForms :: Env -> [SExpr.SExpr] -> InterpM Env
loadTopLevelForms env [] = pure env
loadTopLevelForms env (sx : rest) = do
  env' <- loadTopLevelForm env sx
  loadTopLevelForms env' rest

primitiveEnv :: Env
primitiveEnv = M.fromList
  [ ("__CT-NIL",        MList [])
  , ("__CT-LIFT",       MBuiltin "__CT-LIFT" bCtLift)
  , ("CAR",             MBuiltin "CAR" bCar)
  , ("CDR",             MBuiltin "CDR" bCdr)
  , ("CONS",            MBuiltin "CONS" bCons)
  , ("LIST",            MBuiltin "LIST" bList)
  , ("LENGTH",          MBuiltin "LENGTH" bLength)
  , ("NULL?",           MBuiltin "NULL?" bNullQ)
  , ("SYMBOL?",         MBuiltin "SYMBOL?" bSymbolQ)
  , ("LIST?",           MBuiltin "LIST?" bListQ)
  , ("STRING?",         MBuiltin "STRING?" bStringQ)
  , ("NUMBER?",         MBuiltin "NUMBER?" bNumberQ)
  , ("BOOL?",           MBuiltin "BOOL?" bBoolQ)
  , ("TYPE?",           MBuiltin "TYPE?" bTypeQ)
  , ("EQ",              MBuiltin "EQ" bEq)
  , ("NOT",             MBuiltin "NOT" bNot)
  , ("CONCAT",          MBuiltin "CONCAT" bConcat)
  , ("SYM-TO-STR",      MBuiltin "SYM-TO-STR" bSymToStr)
  , ("STR-TO-SYM",      MBuiltin "STR-TO-SYM" bStrToSym)
  , ("USYM-TO-STR",     MBuiltin "USYM-TO-STR" bUSymToStr)
  , ("STR-TO-USYM",     MBuiltin "STR-TO-USYM" bStrToUSym)
  , ("GENSYM",          MBuiltin "GENSYM" bGensym)
  , ("ERROR",           MBuiltin "ERROR" bError)
  , ("ADD",             MBuiltin "ADD" bAdd)
  , ("SUB",             MBuiltin "SUB" bSub)
  , ("MUL",             MBuiltin "MUL" bMul)
  , ("DIV",             MBuiltin "DIV" bDiv)
  , ("MOD",             MBuiltin "MOD" bMod)
  , ("ADDF",            MBuiltin "ADDF" bAddF)
  , ("SUBF",            MBuiltin "SUBF" bSubF)
  , ("MULF",            MBuiltin "MULF" bMulF)
  , ("DIVF",            MBuiltin "DIVF" bDivF)
  , ("EQI",             MBuiltin "EQI" bEqi)
  , ("LT",              MBuiltin "LT" bLt)
  , ("GT",              MBuiltin "GT" bGt)
  , ("LTI",             MBuiltin "LTI" bLti)
  , ("EQF",             MBuiltin "EQF" bEqf)
  , ("LTF",             MBuiltin "LTF" bLtf)
  , ("EQS",             MBuiltin "EQS" bEqs)
  , ("LTS",             MBuiltin "LTS" bLts)
  , ("STRLEN",          MBuiltin "STRLEN" bStrlen)
  , ("SUBSTR",          MBuiltin "SUBSTR" bSubstr)
  , ("INT-TO-FLT",      MBuiltin "INT-TO-FLT" bIntToFlt)
  , ("FLT-TO-INT",      MBuiltin "FLT-TO-INT" bFltToInt)
  , ("__CT-ATOM",       MBuiltin "__CT-ATOM" bCtAtom)
  , ("__CT-INT",        MBuiltin "__CT-INT" bCtInt)
  , ("__CT-FLT",        MBuiltin "__CT-FLT" bCtFlt)
  , ("__CT-STR",        MBuiltin "__CT-STR" bCtStr)
  , ("__CT-BOOL",       MBuiltin "__CT-BOOL" bCtBool)
  , ("__CT-USYM",       MBuiltin "__CT-USYM" bCtUSym)
  , ("__CT-RX",         MBuiltin "__CT-RX" bCtRx)
  , ("__CT-TYPE",       MBuiltin "__CT-TYPE" bCtType)
  , ("__CT-CONS",       MBuiltin "__CT-CONS" bCons)
  , ("__CT-APPEND",     MBuiltin "__CT-APPEND" bCtAppend)
  ]

-- BUILTINS: LIST OPERATIONS

bCar :: [MVal] -> InterpM MVal
bCar [MList (x:_)] = pure x
bCar [MList []]     = throwError "car: empty list"
bCar [_]            = throwError "car: not a list"
bCar args           = throwError $ "car: expected 1 argument, got " ++ show (length args)

bCdr :: [MVal] -> InterpM MVal
bCdr [MList (_:xs)] = pure $ MList xs
bCdr [MList []]      = throwError "cdr: empty list"
bCdr [_]             = throwError "cdr: not a list"
bCdr args            = throwError $ "cdr: expected 1 argument, got " ++ show (length args)

bCons :: [MVal] -> InterpM MVal
bCons [x, MList xs] = pure $ MList (x : xs)
bCons [_, _]         = throwError "cons: second argument must be a list"
bCons args           = throwError $ "cons: expected 2 arguments, got " ++ show (length args)

bList :: [MVal] -> InterpM MVal
bList args = pure $ MList args

bLength :: [MVal] -> InterpM MVal
bLength [MList xs] = pure $ MInt (fromIntegral (length xs))
bLength [_]         = throwError "length: not a list"
bLength args        = throwError $ "length: expected 1 argument, got " ++ show (length args)

-- BUILTINS: PREDICATES

bNullQ :: [MVal] -> InterpM MVal
bNullQ [MList []] = pure $ MBool True
bNullQ [_]         = pure $ MBool False
bNullQ args        = throwError $ "null?: expected 1 argument, got " ++ show (length args)

bSymbolQ :: [MVal] -> InterpM MVal
bSymbolQ [MAtom _] = pure $ MBool True
bSymbolQ [_]        = pure $ MBool False
bSymbolQ args       = throwError $ "symbol?: expected 1 argument, got " ++ show (length args)

bListQ :: [MVal] -> InterpM MVal
bListQ [MList _] = pure $ MBool True
bListQ [_]        = pure $ MBool False
bListQ args       = throwError $ "list?: expected 1 argument, got " ++ show (length args)

bStringQ :: [MVal] -> InterpM MVal
bStringQ [MStr _] = pure $ MBool True
bStringQ [_]       = pure $ MBool False
bStringQ args      = throwError $ "string?: expected 1 argument, got " ++ show (length args)

bNumberQ :: [MVal] -> InterpM MVal
bNumberQ [MInt _] = pure $ MBool True
bNumberQ [MFlt _] = pure $ MBool True
bNumberQ [_]       = pure $ MBool False
bNumberQ args      = throwError $ "number?: expected 1 argument, got " ++ show (length args)

bBoolQ :: [MVal] -> InterpM MVal
bBoolQ [MBool _] = pure $ MBool True
bBoolQ [_]        = pure $ MBool False
bBoolQ args       = throwError $ "bool?: expected 1 argument, got " ++ show (length args)

bTypeQ :: [MVal] -> InterpM MVal
bTypeQ [MType _] = pure $ MBool True
bTypeQ [_]        = pure $ MBool False
bTypeQ args       = throwError $ "type?: expected 1 argument, got " ++ show (length args)

-- BUILTINS: EQUALITY AND LOGIC

bEq :: [MVal] -> InterpM MVal
bEq [a, b] = pure $ MBool (valEq a b)
bEq args    = throwError $ "eq: expected 2 arguments, got " ++ show (length args)

valEq :: MVal -> MVal -> Bool
valEq (MAtom a)  (MAtom b)  = a == b
valEq (MStr a)   (MStr b)   = a == b
valEq (MInt a)   (MInt b)   = a == b
valEq (MFlt a)   (MFlt b)   = a == b
valEq (MBool a)  (MBool b)  = a == b
valEq (MUSym a)  (MUSym b)  = a == b
valEq (MList a)  (MList b)  = length a == length b && all (uncurry valEq) (zip a b)
valEq (MType a)  (MType b)  = valEq a b
valEq (MQuasi a) (MQuasi b) = valEq a b
valEq (MUnquote a) (MUnquote b) = valEq a b
valEq (MSplice a) (MSplice b) = valEq a b
valEq (MData n xs) (MData m ys) = n == m && length xs == length ys && all (uncurry valEq) (zip xs ys)
valEq _          _          = False

bNot :: [MVal] -> InterpM MVal
bNot [MBool b] = pure $ MBool (not b)
bNot [v]       = pure $ MBool (not (truthy v))
bNot args      = throwError $ "not: expected 1 argument, got " ++ show (length args)

-- BUILTINS: STRING / SYMBOL CONVERSION

bConcat :: [MVal] -> InterpM MVal
bConcat [MStr a, MStr b] = pure $ MStr (T.append a b)
bConcat [_, _]            = throwError "concat: arguments must be strings"
bConcat args              = throwError $ "concat: expected 2 arguments, got " ++ show (length args)

bSymToStr :: [MVal] -> InterpM MVal
bSymToStr [MAtom t] = pure $ MStr t
bSymToStr [_]        = throwError "symbol->string: not a symbol"
bSymToStr args       = throwError $ "symbol->string: expected 1 argument, got " ++ show (length args)

bStrToSym :: [MVal] -> InterpM MVal
bStrToSym [MStr t] = pure $ MAtom t
bStrToSym [_]       = throwError "string->symbol: not a string"
bStrToSym args      = throwError $ "string->symbol: expected 1 argument, got " ++ show (length args)

bUSymToStr :: [MVal] -> InterpM MVal
bUSymToStr [MUSym t] = pure $ MStr t
bUSymToStr [_]       = throwError "usym-to-str: not an uninterned symbol"
bUSymToStr args      = throwError $ "usym-to-str: expected 1 argument, got " ++ show (length args)

bStrToUSym :: [MVal] -> InterpM MVal
bStrToUSym [MStr t] = pure $ MUSym t
bStrToUSym [_]      = throwError "str-to-usym: not a string"
bStrToUSym args     = throwError $ "str-to-usym: expected 1 argument, got " ++ show (length args)

-- BUILTINS: ARITHMETIC

bAdd :: [MVal] -> InterpM MVal
bAdd [MInt a, MInt b] = pure $ MInt (a + b)
bAdd [_, _]            = throwError "add: arguments must be integers"
bAdd args              = throwError $ "add: expected 2 arguments, got " ++ show (length args)

bSub :: [MVal] -> InterpM MVal
bSub [MInt a, MInt b] = pure $ MInt (a - b)
bSub [_, _]            = throwError "sub: arguments must be integers"
bSub args              = throwError $ "sub: expected 2 arguments, got " ++ show (length args)

bMul :: [MVal] -> InterpM MVal
bMul [MInt a, MInt b] = pure $ MInt (a * b)
bMul [_, _]            = throwError "mul: arguments must be integers"
bMul args              = throwError $ "mul: expected 2 arguments, got " ++ show (length args)

bDiv :: [MVal] -> InterpM MVal
bDiv [MInt _, MInt 0]  = throwError "div: division by zero"
bDiv [MInt a, MInt b]  = pure $ MInt (a `div` b)
bDiv [_, _]            = throwError "div: arguments must be integers"
bDiv args              = throwError $ "div: expected 2 arguments, got " ++ show (length args)

bMod :: [MVal] -> InterpM MVal
bMod [MInt _, MInt 0]  = throwError "mod: division by zero"
bMod [MInt a, MInt b]  = pure $ MInt (a `mod` b)
bMod [_, _]            = throwError "mod: arguments must be integers"
bMod args              = throwError $ "mod: expected 2 arguments, got " ++ show (length args)

bAddF :: [MVal] -> InterpM MVal
bAddF [MFlt a, MFlt b] = pure $ MFlt (a + b)
bAddF [_, _]           = throwError "addf: arguments must be floats"
bAddF args             = throwError $ "addf: expected 2 arguments, got " ++ show (length args)

bSubF :: [MVal] -> InterpM MVal
bSubF [MFlt a, MFlt b] = pure $ MFlt (a - b)
bSubF [_, _]           = throwError "subf: arguments must be floats"
bSubF args             = throwError $ "subf: expected 2 arguments, got " ++ show (length args)

bMulF :: [MVal] -> InterpM MVal
bMulF [MFlt a, MFlt b] = pure $ MFlt (a * b)
bMulF [_, _]           = throwError "mulf: arguments must be floats"
bMulF args             = throwError $ "mulf: expected 2 arguments, got " ++ show (length args)

bDivF :: [MVal] -> InterpM MVal
bDivF [MFlt a, MFlt b] = pure $ MFlt (a / b)
bDivF [_, _]           = throwError "divf: arguments must be floats"
bDivF args             = throwError $ "divf: expected 2 arguments, got " ++ show (length args)

bEqi :: [MVal] -> InterpM MVal
bEqi [MInt a, MInt b] = pure $ MBool (a == b)
bEqi [_, _]           = throwError "eqi: arguments must be integers"
bEqi args             = throwError $ "eqi: expected 2 arguments, got " ++ show (length args)

bLt :: [MVal] -> InterpM MVal
bLt [MInt a, MInt b] = pure $ MBool (a < b)
bLt [_, _]            = throwError "lt: arguments must be integers"
bLt args              = throwError $ "lt: expected 2 arguments, got " ++ show (length args)

bGt :: [MVal] -> InterpM MVal
bGt [MInt a, MInt b] = pure $ MBool (a > b)
bGt [_, _]            = throwError "gt: arguments must be integers"
bGt args              = throwError $ "gt: expected 2 arguments, got " ++ show (length args)

bLti :: [MVal] -> InterpM MVal
bLti = bLt

bEqf :: [MVal] -> InterpM MVal
bEqf [MFlt a, MFlt b] = pure $ MBool (a == b)
bEqf [_, _]           = throwError "eqf: arguments must be floats"
bEqf args             = throwError $ "eqf: expected 2 arguments, got " ++ show (length args)

bLtf :: [MVal] -> InterpM MVal
bLtf [MFlt a, MFlt b] = pure $ MBool (a < b)
bLtf [_, _]           = throwError "ltf: arguments must be floats"
bLtf args             = throwError $ "ltf: expected 2 arguments, got " ++ show (length args)

bEqs :: [MVal] -> InterpM MVal
bEqs [MStr a, MStr b] = pure $ MBool (a == b)
bEqs [_, _]           = throwError "eqs: arguments must be strings"
bEqs args             = throwError $ "eqs: expected 2 arguments, got " ++ show (length args)

bLts :: [MVal] -> InterpM MVal
bLts [MStr a, MStr b] = pure $ MBool (a < b)
bLts [_, _]           = throwError "lts: arguments must be strings"
bLts args             = throwError $ "lts: expected 2 arguments, got " ++ show (length args)

bStrlen :: [MVal] -> InterpM MVal
bStrlen [MStr t] = pure $ MInt (fromIntegral (T.length t))
bStrlen [_]      = throwError "strlen: argument must be a string"
bStrlen args     = throwError $ "strlen: expected 1 argument, got " ++ show (length args)

bSubstr :: [MVal] -> InterpM MVal
bSubstr [MStr t, MInt start, MInt len] =
  pure $ MStr (T.take (fromIntegral len) (T.drop (fromIntegral start) t))
bSubstr [_, _, _] = throwError "substr: expected (string int int)"
bSubstr args      = throwError $ "substr: expected 3 arguments, got " ++ show (length args)

bIntToFlt :: [MVal] -> InterpM MVal
bIntToFlt [MInt n] = pure $ MFlt (fromIntegral n)
bIntToFlt [_]      = throwError "int-to-flt: argument must be an integer"
bIntToFlt args     = throwError $ "int-to-flt: expected 1 argument, got " ++ show (length args)

bFltToInt :: [MVal] -> InterpM MVal
bFltToInt [MFlt f] = pure $ MInt (truncate f)
bFltToInt [_]      = throwError "flt-to-int: argument must be a float"
bFltToInt args     = throwError $ "flt-to-int: expected 1 argument, got " ++ show (length args)

-- INTERNAL DATUM CONSTRUCTORS

bCtLift :: [MVal] -> InterpM MVal
bCtLift [MAtom t]    = pure (MAtom t)
bCtLift [MStr t]     = pure (MStr t)
bCtLift [MInt n]     = pure (MInt n)
bCtLift [MFlt f]     = pure (MFlt f)
bCtLift [MList xs]   = pure (MList xs)
bCtLift [MBool b]    = pure (MBool b)
bCtLift [MRx p f]    = pure (MRx p f)
bCtLift [MUSym t]    = pure (MUSym t)
bCtLift [MType v]    = pure (MType v)
bCtLift [MQuasi v]   = pure (MQuasi v)
bCtLift [MUnquote v] = pure (MUnquote v)
bCtLift [MSplice v]  = pure (MSplice v)
bCtLift [v]          = throwError $ "cannot lift value into syntax: " ++ showBrief v
bCtLift args         = throwError $ "__ct-lift: expected 1 argument, got " ++ show (length args)

bCtAtom :: [MVal] -> InterpM MVal
bCtAtom [MStr t] = pure $ MAtom t
bCtAtom [_]      = throwError "__ct-atom: expected a string"
bCtAtom args     = throwError $ "__ct-atom: expected 1 argument, got " ++ show (length args)

bCtInt :: [MVal] -> InterpM MVal
bCtInt [MInt n] = pure $ MInt n
bCtInt [_]      = throwError "__ct-int: expected an integer"
bCtInt args     = throwError $ "__ct-int: expected 1 argument, got " ++ show (length args)

bCtFlt :: [MVal] -> InterpM MVal
bCtFlt [MFlt f] = pure $ MFlt f
bCtFlt [_]      = throwError "__ct-flt: expected a float"
bCtFlt args     = throwError $ "__ct-flt: expected 1 argument, got " ++ show (length args)

bCtStr :: [MVal] -> InterpM MVal
bCtStr [MStr t] = pure $ MStr t
bCtStr [_]      = throwError "__ct-str: expected a string"
bCtStr args     = throwError $ "__ct-str: expected 1 argument, got " ++ show (length args)

bCtBool :: [MVal] -> InterpM MVal
bCtBool [MBool b] = pure $ MBool b
bCtBool [_]       = throwError "__ct-bool: expected a bool"
bCtBool args      = throwError $ "__ct-bool: expected 1 argument, got " ++ show (length args)

bCtUSym :: [MVal] -> InterpM MVal
bCtUSym [MStr t] = pure $ MUSym t
bCtUSym [_]      = throwError "__ct-usym: expected a string"
bCtUSym args     = throwError $ "__ct-usym: expected 1 argument, got " ++ show (length args)

bCtRx :: [MVal] -> InterpM MVal
bCtRx [MStr p, MStr f] = pure $ MRx p f
bCtRx [_, _]           = throwError "__ct-rx: expected (string string)"
bCtRx args             = throwError $ "__ct-rx: expected 2 arguments, got " ++ show (length args)

bCtType :: [MVal] -> InterpM MVal
bCtType [v] = pure $ MType v
bCtType args = throwError $ "__ct-type: expected 1 argument, got " ++ show (length args)

bCtAppend :: [MVal] -> InterpM MVal
bCtAppend [MList xs, MList ys] = pure $ MList (xs ++ ys)
bCtAppend [_, _]               = throwError "__ct-append: arguments must be lists"
bCtAppend args                 = throwError $ "__ct-append: expected 2 arguments, got " ++ show (length args)

-- BUILTINS: GENSYM

bGensym :: [MVal] -> InterpM MVal
bGensym [] = do
  n <- State.get
  State.put (n + 1)
  pure $ MAtom (T.pack ("__G" ++ show n))
bGensym args = throwError $ "gensym: expected 0 arguments, got " ++ show (length args)

-- BUILTINS: ERROR

bError :: [MVal] -> InterpM MVal
bError [MStr msg] = throwError $ "macro error: " ++ T.unpack msg
bError [val]      = throwError $ "macro error: " ++ showBrief val
bError args       = throwError $ "error: expected 1 argument, got " ++ show (length args)

-- HELPERS

showBrief :: MVal -> String
showBrief (MAtom t)    = T.unpack t
showBrief (MStr t)     = show t
showBrief (MInt n)     = show n
showBrief (MFlt f)     = show f
showBrief (MList _)    = "<list>"
showBrief (MBool b)    = show b
showBrief (MRx _ _)    = "<regex>"
showBrief (MUSym t)    = ":" ++ T.unpack t
showBrief (MType _)    = "<type>"
showBrief (MQuasi _)   = "<quasi>"
showBrief (MUnquote _) = "<unquote>"
showBrief (MSplice _)  = "<splice>"
showBrief (MData n _)  = T.unpack n
showBrief (MCtor n _ _) = "<ctor:" ++ T.unpack n ++ ">"
showBrief (MClosure {}) = "<closure>"
showBrief (MTypedClosure {}) = "<typed-closure>"
showBrief (MBuiltin n _) = "<builtin:" ++ T.unpack n ++ ">"

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
