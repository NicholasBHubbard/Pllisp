{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.MacroInterp
  ( MVal(..)
  , Env
  , InterpM
  , evalTyped
  , runInterpM
  , defaultEnv
  , loadTypedTopLevelBindings
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
import Text.Regex.TDFA ((=~))

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
  | MRef Int
  | MData T.Text [MVal]
  | MCtor T.Text Int [MVal]
  | MUnavailable T.Text String
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
  show (MRef n) = "MRef " ++ show n
  show (MData n xs) = "MData " ++ show n ++ " " ++ show xs
  show (MCtor n _ xs) = "MCtor " ++ show n ++ " " ++ show xs
  show (MUnavailable n msg) = "MUnavailable " ++ show n ++ " " ++ show msg
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
  MRef a    == MRef b    = a == b
  MData n xs == MData m ys = n == m && xs == ys
  _         == _         = False

type Env = M.Map T.Text MVal

data InterpState = InterpState
  { isGensym :: Int
  , isNextRef :: Int
  , isRefs :: M.Map Int MVal
  }

type InterpM = State.StateT InterpState (Either String)

throwError :: String -> InterpM a
throwError = State.StateT . const . Left

runInterpM :: InterpM a -> Either String a
runInterpM m = State.evalStateT m (InterpState 0 0 M.empty)

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
  MRef {} -> Left "cannot convert ref to syntax"
  MData {} -> Left "cannot convert data value to syntax"
  MCtor {} -> Left "cannot convert constructor to syntax"
  MUnavailable _ msg -> Left msg
  MTypedClosure {} -> Left "cannot convert typed closure to syntax"
  MBuiltin {} -> Left "cannot convert builtin to syntax"
  where
    loc = Loc.Located dummySpan

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

loadTypedTopLevelBindings :: Env -> [TC.TRExpr] -> InterpM Env
loadTypedTopLevelBindings env [] = pure env
loadTypedTopLevelBindings env (expr : rest) = do
  env' <- loadTypedTopLevelBinding env expr
  loadTypedTopLevelBindings env' rest

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

loadTypedTopLevelBinding :: Env -> TC.TRExpr -> InterpM Env
loadTypedTopLevelBinding env (Loc.Located _ (Ty.Typed _ expr)) = case expr of
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
  TC.TRLet binds _ ->
    bindTypedSequential env binds
  _ ->
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
ffiStub name = MUnavailable name ("ffi not available at macro expansion time: " ++ T.unpack name)

litToVal :: CST.Literal -> MVal
litToVal lit = case lit of
  CST.LitInt n -> MInt n
  CST.LitFlt f -> MFlt f
  CST.LitStr t -> MStr t
  CST.LitRx p f -> MRx p f
  CST.LitUSym t -> MUSym t

-- FUNCTION APPLICATION

apply :: MVal -> [MVal] -> InterpM MVal
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
apply (MUnavailable _ msg) _ = throwError msg
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

primitiveEnv :: Env
primitiveEnv = M.fromList
  [ ("SYNTAX-EMPTY",    MList [])
  , ("SYNTAX-LIFT",     MBuiltin "SYNTAX-LIFT" bSyntaxLift)
  , ("SYNTAX-CAR",      MBuiltin "SYNTAX-CAR" bCar)
  , ("SYNTAX-CDR",      MBuiltin "SYNTAX-CDR" bCdr)
  , ("SYNTAX-LENGTH",   MBuiltin "SYNTAX-LENGTH" bLength)
  , ("SYNTAX-NULL?",    MBuiltin "SYNTAX-NULL?" bNullQ)
  , ("SYNTAX-SYMBOL?",  MBuiltin "SYNTAX-SYMBOL?" bSymbolQ)
  , ("SYNTAX-LIST?",    MBuiltin "SYNTAX-LIST?" bListQ)
  , ("SYNTAX-STRING?",  MBuiltin "SYNTAX-STRING?" bStringQ)
  , ("SYNTAX-NUMBER?",  MBuiltin "SYNTAX-NUMBER?" bNumberQ)
  , ("SYNTAX-BOOL?",    MBuiltin "SYNTAX-BOOL?" bBoolQ)
  , ("SYNTAX-TYPE?",    MBuiltin "SYNTAX-TYPE?" bTypeQ)
  , ("EQ",              MBuiltin "EQ" bEq)
  , ("NOT",             MBuiltin "NOT" bNot)
  , ("CONCAT",          MBuiltin "CONCAT" bConcat)
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
  , ("REF",             MBuiltin "REF" bRef)
  , ("DEREF",           MBuiltin "DEREF" bDeref)
  , ("SET!",            MBuiltin "SET!" bSetRef)
  , ("RX-COMPILE",      MBuiltin "RX-COMPILE" bRxCompile)
  , ("RX-MATCH",        MBuiltin "RX-MATCH" bRxMatch)
  , ("RX-FIND",         MBuiltin "RX-FIND" bRxFind)
  , ("RX-SUB",          MBuiltin "RX-SUB" bRxSub)
  , ("RX-GSUB",         MBuiltin "RX-GSUB" bRxGSub)
  , ("RX-SPLIT",        MBuiltin "RX-SPLIT" bRxSplit)
  , ("RX-CAPTURES",     MBuiltin "RX-CAPTURES" bRxCaptures)
  , ("SYNTAX-SYMBOL",   MBuiltin "SYNTAX-SYMBOL" bSyntaxSymbol)
  , ("SYNTAX-INT",      MBuiltin "SYNTAX-INT" bSyntaxInt)
  , ("SYNTAX-FLOAT",    MBuiltin "SYNTAX-FLOAT" bSyntaxFloat)
  , ("SYNTAX-STRING",   MBuiltin "SYNTAX-STRING" bSyntaxString)
  , ("SYNTAX-BOOL",     MBuiltin "SYNTAX-BOOL" bSyntaxBool)
  , ("SYNTAX-USYM",     MBuiltin "SYNTAX-USYM" bSyntaxUSym)
  , ("SYNTAX-RX",       MBuiltin "SYNTAX-RX" bSyntaxRx)
  , ("SYNTAX-TYPE",     MBuiltin "SYNTAX-TYPE" bSyntaxType)
  , ("SYNTAX-CONS",     MBuiltin "SYNTAX-CONS" bCons)
  , ("SYNTAX-APPEND",   MBuiltin "SYNTAX-APPEND" bSyntaxAppend)
  , ("SYNTAX-INT-VALUE", MBuiltin "SYNTAX-INT-VALUE" bSyntaxIntValue)
  , ("SYNTAX-FLOAT-VALUE", MBuiltin "SYNTAX-FLOAT-VALUE" bSyntaxFltValue)
  , ("SYNTAX-STRING-VALUE", MBuiltin "SYNTAX-STRING-VALUE" bSyntaxStrValue)
  , ("SYNTAX-SYMBOL-NAME", MBuiltin "SYNTAX-SYMBOL-NAME" bSymToStr)
  , ("SYNTAX-USYM-NAME", MBuiltin "SYNTAX-USYM-NAME" bUSymToStr)
  ]

-- SYNTAX HELPERS: LIST OPERATIONS

bCar :: [MVal] -> InterpM MVal
bCar [MList (x:_)] = pure x
bCar [MList []]     = throwError "syntax-car: empty list"
bCar [_]            = throwError "syntax-car: not a list"
bCar args           = throwError $ "syntax-car: expected 1 argument, got " ++ show (length args)

bCdr :: [MVal] -> InterpM MVal
bCdr [MList (_:xs)] = pure $ MList xs
bCdr [MList []]      = throwError "syntax-cdr: empty list"
bCdr [_]             = throwError "syntax-cdr: not a list"
bCdr args            = throwError $ "syntax-cdr: expected 1 argument, got " ++ show (length args)

bCons :: [MVal] -> InterpM MVal
bCons [x, MList xs] = pure $ MList (x : xs)
bCons [_, _]         = throwError "syntax-cons: second argument must be a list"
bCons args           = throwError $ "syntax-cons: expected 2 arguments, got " ++ show (length args)

bLength :: [MVal] -> InterpM MVal
bLength [MList xs] = pure $ MInt (fromIntegral (length xs))
bLength [_]         = throwError "syntax-length: not a list"
bLength args        = throwError $ "syntax-length: expected 1 argument, got " ++ show (length args)

-- SYNTAX HELPERS: PREDICATES

bNullQ :: [MVal] -> InterpM MVal
bNullQ [MList []] = pure $ MBool True
bNullQ [_]         = pure $ MBool False
bNullQ args        = throwError $ "syntax-null?: expected 1 argument, got " ++ show (length args)

bSymbolQ :: [MVal] -> InterpM MVal
bSymbolQ [MAtom _] = pure $ MBool True
bSymbolQ [_]        = pure $ MBool False
bSymbolQ args       = throwError $ "syntax-symbol?: expected 1 argument, got " ++ show (length args)

bListQ :: [MVal] -> InterpM MVal
bListQ [MList _] = pure $ MBool True
bListQ [_]        = pure $ MBool False
bListQ args       = throwError $ "syntax-list?: expected 1 argument, got " ++ show (length args)

bStringQ :: [MVal] -> InterpM MVal
bStringQ [MStr _] = pure $ MBool True
bStringQ [_]       = pure $ MBool False
bStringQ args      = throwError $ "syntax-string?: expected 1 argument, got " ++ show (length args)

bNumberQ :: [MVal] -> InterpM MVal
bNumberQ [MInt _] = pure $ MBool True
bNumberQ [MFlt _] = pure $ MBool True
bNumberQ [_]       = pure $ MBool False
bNumberQ args      = throwError $ "syntax-number?: expected 1 argument, got " ++ show (length args)

bBoolQ :: [MVal] -> InterpM MVal
bBoolQ [MBool _] = pure $ MBool True
bBoolQ [_]        = pure $ MBool False
bBoolQ args       = throwError $ "syntax-bool?: expected 1 argument, got " ++ show (length args)

bTypeQ :: [MVal] -> InterpM MVal
bTypeQ [MType _] = pure $ MBool True
bTypeQ [_]        = pure $ MBool False
bTypeQ args       = throwError $ "syntax-type?: expected 1 argument, got " ++ show (length args)

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

-- SYNTAX HELPERS: STRING / SYMBOL CONVERSION

bConcat :: [MVal] -> InterpM MVal
bConcat [MStr a, MStr b] = pure $ MStr (T.append a b)
bConcat [_, _]            = throwError "concat: arguments must be strings"
bConcat args              = throwError $ "concat: expected 2 arguments, got " ++ show (length args)

bSymToStr :: [MVal] -> InterpM MVal
bSymToStr [MAtom t] = pure $ MStr t
bSymToStr [_]        = throwError "syntax-symbol-name: not symbol syntax"
bSymToStr args       = throwError $ "syntax-symbol-name: expected 1 argument, got " ++ show (length args)

bUSymToStr :: [MVal] -> InterpM MVal
bUSymToStr [MUSym t] = pure $ MStr t
bUSymToStr [_]       = throwError "syntax-usym-name: not usym syntax"
bUSymToStr args      = throwError $ "syntax-usym-name: expected 1 argument, got " ++ show (length args)

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

-- BUILTINS: REFS

bRef :: [MVal] -> InterpM MVal
bRef [val] = do
  st <- State.get
  let refId = isNextRef st
  State.put st { isNextRef = refId + 1, isRefs = M.insert refId val (isRefs st) }
  pure (MRef refId)
bRef args = throwError $ "ref: expected 1 argument, got " ++ show (length args)

bDeref :: [MVal] -> InterpM MVal
bDeref [MRef refId] = do
  refs <- isRefs <$> State.get
  case M.lookup refId refs of
    Just val -> pure val
    Nothing -> throwError "deref: unknown ref"
bDeref [_] = throwError "deref: argument must be a ref"
bDeref args = throwError $ "deref: expected 1 argument, got " ++ show (length args)

bSetRef :: [MVal] -> InterpM MVal
bSetRef [MRef refId, val] = do
  st <- State.get
  State.put st { isRefs = M.insert refId val (isRefs st) }
  pure (MAtom "UNIT")
bSetRef [_, _] = throwError "set!: first argument must be a ref"
bSetRef args = throwError $ "set!: expected 2 arguments, got " ++ show (length args)

-- BUILTINS: REGEX

bRxCompile :: [MVal] -> InterpM MVal
bRxCompile [MStr pat] = pure (MRx pat "")
bRxCompile [_] = throwError "rx-compile: argument must be a string"
bRxCompile args = throwError $ "rx-compile: expected 1 argument, got " ++ show (length args)

bRxMatch :: [MVal] -> InterpM MVal
bRxMatch [MRx pat _, MStr input] =
  pure (MBool (regexMatches pat input))
bRxMatch [_, _] = throwError "rx-match: expected (regex string)"
bRxMatch args = throwError $ "rx-match: expected 2 arguments, got " ++ show (length args)

bRxFind :: [MVal] -> InterpM MVal
bRxFind [MRx pat _, MStr input] =
  pure (MStr (maybe "" (\(m, _, _, _) -> m) (firstRegexMatch pat input)))
bRxFind [_, _] = throwError "rx-find: expected (regex string)"
bRxFind args = throwError $ "rx-find: expected 2 arguments, got " ++ show (length args)

bRxSub :: [MVal] -> InterpM MVal
bRxSub [MRx pat _, MStr repl, MStr input] =
  pure (MStr (regexSub False pat repl input))
bRxSub [_, _, _] = throwError "rx-sub: expected (regex string string)"
bRxSub args = throwError $ "rx-sub: expected 3 arguments, got " ++ show (length args)

bRxGSub :: [MVal] -> InterpM MVal
bRxGSub [MRx pat _, MStr repl, MStr input] =
  pure (MStr (regexSub True pat repl input))
bRxGSub [_, _, _] = throwError "rx-gsub: expected (regex string string)"
bRxGSub args = throwError $ "rx-gsub: expected 3 arguments, got " ++ show (length args)

bRxSplit :: [MVal] -> InterpM MVal
bRxSplit [MRx pat _, MStr input] =
  pure (MList (map MStr (regexSplit pat input)))
bRxSplit [_, _] = throwError "rx-split: expected (regex string)"
bRxSplit args = throwError $ "rx-split: expected 2 arguments, got " ++ show (length args)

bRxCaptures :: [MVal] -> InterpM MVal
bRxCaptures [MRx pat _, MStr input] =
  pure (MList (map MStr (regexCaptures pat input)))
bRxCaptures [_, _] = throwError "rx-captures: expected (regex string)"
bRxCaptures args = throwError $ "rx-captures: expected 2 arguments, got " ++ show (length args)

-- SYNTAX HELPERS

bSyntaxLift :: [MVal] -> InterpM MVal
bSyntaxLift [MAtom t]    = pure (MAtom t)
bSyntaxLift [MStr t]     = pure (MStr t)
bSyntaxLift [MInt n]     = pure (MInt n)
bSyntaxLift [MFlt f]     = pure (MFlt f)
bSyntaxLift [MList xs]   = pure (MList xs)
bSyntaxLift [MBool b]    = pure (MBool b)
bSyntaxLift [MRx p f]    = pure (MRx p f)
bSyntaxLift [MUSym t]    = pure (MUSym t)
bSyntaxLift [MType v]    = pure (MType v)
bSyntaxLift [MQuasi v]   = pure (MQuasi v)
bSyntaxLift [MUnquote v] = pure (MUnquote v)
bSyntaxLift [MSplice v]  = pure (MSplice v)
bSyntaxLift [v]          = throwError $ "cannot lift value into syntax: " ++ showBrief v
bSyntaxLift args         = throwError $ "syntax-lift: expected 1 argument, got " ++ show (length args)

bSyntaxSymbol :: [MVal] -> InterpM MVal
bSyntaxSymbol [MStr t] = pure $ MAtom t
bSyntaxSymbol [_]      = throwError "syntax-symbol: expected a string"
bSyntaxSymbol args     = throwError $ "syntax-symbol: expected 1 argument, got " ++ show (length args)

bSyntaxInt :: [MVal] -> InterpM MVal
bSyntaxInt [MInt n] = pure $ MInt n
bSyntaxInt [_]      = throwError "syntax-int: expected an integer"
bSyntaxInt args     = throwError $ "syntax-int: expected 1 argument, got " ++ show (length args)

bSyntaxFloat :: [MVal] -> InterpM MVal
bSyntaxFloat [MFlt f] = pure $ MFlt f
bSyntaxFloat [_]      = throwError "syntax-float: expected a float"
bSyntaxFloat args     = throwError $ "syntax-float: expected 1 argument, got " ++ show (length args)

bSyntaxString :: [MVal] -> InterpM MVal
bSyntaxString [MStr t] = pure $ MStr t
bSyntaxString [_]      = throwError "syntax-string: expected a string"
bSyntaxString args     = throwError $ "syntax-string: expected 1 argument, got " ++ show (length args)

bSyntaxBool :: [MVal] -> InterpM MVal
bSyntaxBool [MBool b] = pure $ MBool b
bSyntaxBool [_]       = throwError "syntax-bool: expected a bool"
bSyntaxBool args      = throwError $ "syntax-bool: expected 1 argument, got " ++ show (length args)

bSyntaxUSym :: [MVal] -> InterpM MVal
bSyntaxUSym [MStr t] = pure $ MUSym t
bSyntaxUSym [_]      = throwError "syntax-usym: expected a string"
bSyntaxUSym args     = throwError $ "syntax-usym: expected 1 argument, got " ++ show (length args)

bSyntaxRx :: [MVal] -> InterpM MVal
bSyntaxRx [MStr p, MStr f] = pure $ MRx p f
bSyntaxRx [_, _]           = throwError "syntax-rx: expected (string string)"
bSyntaxRx args             = throwError $ "syntax-rx: expected 2 arguments, got " ++ show (length args)

bSyntaxType :: [MVal] -> InterpM MVal
bSyntaxType [v] = pure $ MType v
bSyntaxType args = throwError $ "syntax-type: expected 1 argument, got " ++ show (length args)

bSyntaxAppend :: [MVal] -> InterpM MVal
bSyntaxAppend [MList xs, MList ys] = pure $ MList (xs ++ ys)
bSyntaxAppend [_, _]               = throwError "syntax-append: arguments must be lists"
bSyntaxAppend args                 = throwError $ "syntax-append: expected 2 arguments, got " ++ show (length args)

bSyntaxIntValue :: [MVal] -> InterpM MVal
bSyntaxIntValue [MInt n] = pure $ MInt n
bSyntaxIntValue [_]      = throwError "syntax-int-value: expected integer syntax"
bSyntaxIntValue args     = throwError $ "syntax-int-value: expected 1 argument, got " ++ show (length args)

bSyntaxFltValue :: [MVal] -> InterpM MVal
bSyntaxFltValue [MFlt f] = pure $ MFlt f
bSyntaxFltValue [_]      = throwError "syntax-float-value: expected float syntax"
bSyntaxFltValue args     = throwError $ "syntax-float-value: expected 1 argument, got " ++ show (length args)

bSyntaxStrValue :: [MVal] -> InterpM MVal
bSyntaxStrValue [MStr t] = pure $ MStr t
bSyntaxStrValue [_]      = throwError "syntax-string-value: expected string syntax"
bSyntaxStrValue args     = throwError $ "syntax-string-value: expected 1 argument, got " ++ show (length args)

-- BUILTINS: GENSYM

bGensym :: [MVal] -> InterpM MVal
bGensym [] = do
  st <- State.get
  let n = isGensym st
  State.put st { isGensym = n + 1 }
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
showBrief (MRef n)     = "<ref:" ++ show n ++ ">"
showBrief (MData n _)  = T.unpack n
showBrief (MCtor n _ _) = "<ctor:" ++ T.unpack n ++ ">"
showBrief (MUnavailable n _) = "<unavailable:" ++ T.unpack n ++ ">"
showBrief (MTypedClosure {}) = "<typed-closure>"
showBrief (MBuiltin n _) = "<builtin:" ++ T.unpack n ++ ">"

regexMatches :: T.Text -> T.Text -> Bool
regexMatches pat input =
  T.unpack input =~ T.unpack pat

firstRegexMatch :: T.Text -> T.Text -> Maybe (T.Text, [T.Text], T.Text, T.Text)
firstRegexMatch pat input =
  let (pre, match, post, groups) =
        T.unpack input =~ T.unpack pat :: (String, String, String, [String])
  in if null match
       then Nothing
       else Just (T.pack match, map T.pack groups, T.pack pre, T.pack post)

regexCaptures :: T.Text -> T.Text -> [T.Text]
regexCaptures pat input =
  case firstRegexMatch pat input of
    Just (_, groups, _, _) -> groups
    Nothing -> []

regexSub :: Bool -> T.Text -> T.Text -> T.Text -> T.Text
regexSub global pat repl input =
  go input
  where
    go txt = case firstRegexMatch pat txt of
      Nothing -> txt
      Just (_, groups, pre, post) ->
        let replaced = expandReplacement repl groups
            out = pre <> replaced
        in if global
             then out <> go post
             else out <> post

regexSplit :: T.Text -> T.Text -> [T.Text]
regexSplit pat input =
  go input
  where
    go txt = case firstRegexMatch pat txt of
      Nothing -> [txt]
      Just (_, _, pre, post) -> pre : go post

expandReplacement :: T.Text -> [T.Text] -> T.Text
expandReplacement repl groups =
  T.pack (go (T.unpack repl))
  where
    go [] = []
    go ('\\':d:rest)
      | d >= '0' && d <= '9' =
          let idx = fromEnum d - fromEnum '0'
              grp = if idx >= 1 && idx <= length groups then T.unpack (groups !! (idx - 1)) else ""
          in grp ++ go rest
    go (c:rest) = c : go rest

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
