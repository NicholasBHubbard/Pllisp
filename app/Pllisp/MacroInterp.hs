{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.MacroInterp
  ( MVal(..)
  , Env
  , InterpM
  , eval
  , runInterpM
  , defaultEnv
  , loadTopLevelForm
  , loadTopLevelForms
  , sexprToVal
  , valToSExpr
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Control.Monad.State.Strict as State

import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.SrcLoc as Loc

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
  | MClosure Env [T.Text] SExpr.SExpr
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
  show (MClosure _ ps _) = "MClosure <" ++ show ps ++ ">"
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
  SExpr.SQuasi inner  -> MList [MAtom "%QUASI", sexprToVal inner]
  SExpr.SUnquote inner -> MList [MAtom "%UNQUOTE", sexprToVal inner]
  SExpr.SSplice inner -> MList [MAtom "%SPLICE", sexprToVal inner]

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
  MClosure {} -> Left "cannot convert closure to syntax"
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

-- FUNCTION APPLICATION

apply :: MVal -> [MVal] -> InterpM MVal
apply (MClosure closureEnv params body) args
  | length params == length args = do
      let env = M.union (M.fromList (zip params args)) closureEnv
      eval env body
  | otherwise = throwError $ "wrong number of arguments: expected "
      ++ show (length params) ++ ", got " ++ show (length args)
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
  [ ("CAR",             MBuiltin "CAR" bCar)
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
  , ("LT",              MBuiltin "LT" bLt)
  , ("GT",              MBuiltin "GT" bGt)
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

bLt :: [MVal] -> InterpM MVal
bLt [MInt a, MInt b] = pure $ MBool (a < b)
bLt [_, _]            = throwError "lt: arguments must be integers"
bLt args              = throwError $ "lt: expected 2 arguments, got " ++ show (length args)

bGt :: [MVal] -> InterpM MVal
bGt [MInt a, MInt b] = pure $ MBool (a > b)
bGt [_, _]            = throwError "gt: arguments must be integers"
bGt args              = throwError $ "gt: expected 2 arguments, got " ++ show (length args)

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
showBrief (MClosure {}) = "<closure>"
showBrief (MBuiltin n _) = "<builtin:" ++ T.unpack n ++ ">"

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
