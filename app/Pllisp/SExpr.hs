{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.SExpr where

import qualified Data.Text as T

import qualified Pllisp.CST    as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type   as Ty

-- CORE

type SExpr = Loc.Located SExprF

data SExprF
  = SAtom T.Text
  | SInt Integer
  | SFlt Double
  | SStr T.Text
  | SRx T.Text T.Text
  | SList [SExpr]
  | SType SExpr
  | SQuasi SExpr
  | SUnquote SExpr
  | SSplice SExpr
  deriving (Eq, Show)

-- PRE-SCAN

-- | Scan raw SExprs for (module ...) form before macro expansion.
preScanModuleName :: [SExpr] -> Maybe T.Text
preScanModuleName [] = Nothing
preScanModuleName (Loc.Located _ (SList [Loc.Located _ (SAtom "MODULE"), Loc.Located _ (SAtom name)]) : _) = Just name
preScanModuleName (_ : rest) = preScanModuleName rest

-- | Scan raw SExprs for (import ...) forms before macro expansion.
preScanImports :: [SExpr] -> [CST.Import]
preScanImports [] = []
preScanImports (sx@(Loc.Located _ (SList (Loc.Located _ (SAtom "IMPORT") : _))) : rest) =
  case toImport sx of
    Right imp -> imp : preScanImports rest
    Left _    -> preScanImports rest
preScanImports (_ : rest) = preScanImports rest

-- SEXPR → CST CONVERSION

data ConvertError = ConvertError
  { ceSpan :: Loc.Span
  , ceMsg  :: String
  } deriving (Eq, Show)

toProgram :: [SExpr] -> Either ConvertError CST.Program
toProgram sexprs = do
  let (mName, rest1) = takeModule sexprs
      (imports, rest2) = partitionImports rest1
  exprs <- mapM toExpr rest2
  pure $ CST.Program mName imports exprs

takeModule :: [SExpr] -> (Maybe T.Text, [SExpr])
takeModule = go []
  where
    go before (Loc.Located _ (SList [Loc.Located _ (SAtom "MODULE"), Loc.Located _ (SAtom name)]) : rest) =
      (Just name, reverse before ++ rest)
    go before (sx : rest) = go (sx : before) rest
    go before [] = (Nothing, reverse before)

-- | Partition SExprs into imports and non-imports, scanning all forms.
partitionImports :: [SExpr] -> ([CST.Import], [SExpr])
partitionImports = go [] []
  where
    go imps other [] = (reverse imps, reverse other)
    go imps other (sx@(Loc.Located _ (SList (Loc.Located _ (SAtom "IMPORT") : _))) : rest) =
      case toImport sx of
        Right imp -> go (imp : imps) other rest
        Left _    -> go imps (sx : other) rest
    go imps other (sx : rest) = go imps (sx : other) rest

toImport :: SExpr -> Either ConvertError CST.Import
-- (import FOO)
toImport (Loc.Located _ (SList [Loc.Located _ (SAtom "IMPORT"), Loc.Located _ (SAtom modName)])) =
  Right $ CST.Import modName modName []
-- (import FOO (x y))
toImport (Loc.Located _ (SList [Loc.Located _ (SAtom "IMPORT"), Loc.Located _ (SAtom modName), Loc.Located _ (SList unquals)])) = do
  names <- mapM toAtomName unquals
  Right $ CST.Import modName modName names
-- (import FOO F)
toImport (Loc.Located _ (SList [Loc.Located _ (SAtom "IMPORT"), Loc.Located _ (SAtom modName), Loc.Located _ (SAtom alias)])) =
  Right $ CST.Import modName alias []
-- (import FOO F (x y))
toImport (Loc.Located _ (SList [Loc.Located _ (SAtom "IMPORT"), Loc.Located _ (SAtom modName), Loc.Located _ (SAtom alias), Loc.Located _ (SList unquals)])) = do
  names <- mapM toAtomName unquals
  Right $ CST.Import modName alias names
toImport (Loc.Located sp _) = Left $ ConvertError sp "invalid import"

toAtomName :: SExpr -> Either ConvertError T.Text
toAtomName (Loc.Located _ (SAtom name)) = Right name
toAtomName (Loc.Located sp _) = Left $ ConvertError sp "expected symbol"

-- EXPRESSIONS

toExpr :: SExpr -> Either ConvertError CST.Expr
toExpr (Loc.Located sp sexprF) = case sexprF of
  SInt n   -> Right $ Loc.Located sp (CST.ExprLit (CST.LitInt n))
  SFlt d   -> Right $ Loc.Located sp (CST.ExprLit (CST.LitFlt d))
  SStr s   -> Right $ Loc.Located sp (CST.ExprLit (CST.LitStr s))
  SRx p f  -> Right $ Loc.Located sp (CST.ExprLit (CST.LitRx p f))

  SAtom "TRUE"  -> Right $ Loc.Located sp (CST.ExprBool True)
  SAtom "FALSE" -> Right $ Loc.Located sp (CST.ExprBool False)
  SAtom "UNIT"  -> Right $ Loc.Located sp CST.ExprUnit
  SAtom s       -> Right $ Loc.Located sp (CST.ExprSym s)

  SList (Loc.Located _ (SAtom "LAM")  : rest) -> Loc.Located sp <$> toLam sp rest
  SList (Loc.Located _ (SAtom "LET")  : rest) -> Loc.Located sp <$> toLet sp rest
  SList (Loc.Located _ (SAtom "IF")   : rest) -> Loc.Located sp <$> toIf sp rest
  SList (Loc.Located _ (SAtom "TYPE") : rest) -> Loc.Located sp <$> toTypeDecl sp rest
  SList (Loc.Located _ (SAtom "CASE") : rest) -> Loc.Located sp <$> toCase sp rest
  SList (Loc.Located _ (SAtom "CLS")  : rest) -> Loc.Located sp <$> toCls sp rest
  SList (Loc.Located _ (SAtom "INST") : rest) -> Loc.Located sp <$> toInst sp rest
  SList (Loc.Located _ (SAtom "FFI")  : rest) -> Loc.Located sp <$> toFFIDecl sp rest
  SList (Loc.Located _ (SAtom "FFI-STRUCT") : rest) -> Loc.Located sp <$> toFFIStruct sp rest
  SList (Loc.Located _ (SAtom "FFI-VAR") : rest) -> Loc.Located sp <$> toFFIVar sp rest
  SList (Loc.Located _ (SAtom "FFI-ENUM") : rest) -> Loc.Located sp <$> toFFIEnum sp rest
  SList (Loc.Located _ (SAtom "FFI-CALLBACK") : rest) -> Loc.Located sp <$> toFFICallback sp rest
  SList [Loc.Located _ (SAtom dotName), arg]
    | Just field <- T.stripPrefix "." dotName -> do
        arg' <- toExpr arg
        Right $ Loc.Located sp (CST.ExprFieldAccess field arg')
  SList (fun : args)
    | any isKeyArg args -> do
        fun' <- toExpr fun
        args' <- processKeyArgs sp args
        Right $ Loc.Located sp (CST.ExprApp fun' args')
    | otherwise -> do
        fun'  <- toExpr fun
        args' <- mapM toExpr args
        Right $ Loc.Located sp (CST.ExprApp fun' args')
  SList [] -> Right $ Loc.Located sp CST.ExprUnit

  SType _     -> Left $ ConvertError sp "unexpected type annotation in expression position"
  SQuasi _    -> Left $ ConvertError sp "quasiquote outside macro body"
  SUnquote _  -> Left $ ConvertError sp "unquote outside quasiquote"
  SSplice _   -> Left $ ConvertError sp "splice outside quasiquote"

-- LAMBDA

toLam :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toLam sp [Loc.Located _ (SList params), Loc.Located _ (SType retTy), body] = do
  lamList <- toLamList sp params
  ty      <- toType retTy
  body'   <- toExpr body
  Right $ CST.ExprLam lamList (Just ty) body'
toLam sp [Loc.Located _ (SList params), body] = do
  lamList <- toLamList sp params
  body'   <- toExpr body
  Right $ CST.ExprLam lamList Nothing body'
toLam sp _ = Left $ ConvertError sp "invalid lambda: expected (lam (params...) [%type] body)"

-- | Parse an extended lambda list: required params, then optionally &rest, %opt, or &key.
toLamList :: Loc.Span -> [SExpr] -> Either ConvertError CST.LamList
toLamList sp sexprs = do
  let (reqSexprs, rest) = span (not . isLamMarker) sexprs
  required <- mapM toTSymbol reqSexprs
  extra <- case rest of
    [] -> Right CST.NoExtra

    -- &rest param
    (Loc.Located _ (SAtom "&REST") : afterRest) -> do
      rejectTrailingMarkers sp afterRest
      case afterRest of
        [p] -> do
          tsym <- toTSymbol p
          Right $ CST.RestParam tsym
        [] -> Left $ ConvertError sp "&rest must be followed by a parameter"
        _  -> Left $ ConvertError sp "&rest must be followed by exactly one parameter"

    -- %opt (param default)...
    (Loc.Located _ (SType (Loc.Located _ (SAtom "OPT"))) : afterOpt) -> do
      rejectTrailingMarkers sp afterOpt
      opts <- mapM (toDefaultParam sp "%opt") afterOpt
      Right $ CST.OptParams opts

    -- &key (param default)...
    (Loc.Located _ (SAtom "&KEY") : afterKey) -> do
      rejectTrailingMarkers sp afterKey
      keys <- mapM (toDefaultParam sp "&key") afterKey
      Right $ CST.KeyParams keys

    (Loc.Located sp' _ : _) ->
      Left $ ConvertError sp' "unexpected marker in lambda list"

  Right $ CST.LamList required extra

-- | Check if an SExpr is a lambda list marker (&REST, &KEY, or %OPT).
isLamMarker :: SExpr -> Bool
isLamMarker (Loc.Located _ (SAtom "&REST")) = True
isLamMarker (Loc.Located _ (SAtom "&KEY"))  = True
isLamMarker (Loc.Located _ (SType (Loc.Located _ (SAtom "OPT")))) = True
isLamMarker _ = False

-- | Reject if there are additional markers after the first one (no mixing).
rejectTrailingMarkers :: Loc.Span -> [SExpr] -> Either ConvertError ()
rejectTrailingMarkers sp sexprs =
  case filter isLamMarker sexprs of
    [] -> Right ()
    _  -> Left $ ConvertError sp "cannot mix &rest, %opt, and &key in lambda list"

-- | Parse a (param default) pair for %opt or &key.
toDefaultParam :: Loc.Span -> String -> SExpr -> Either ConvertError (CST.TSymbol, CST.Expr)
toDefaultParam sp label (Loc.Located _ (SList [name, val])) = do
  tsym <- toTSymbol name
  val' <- toExpr val
  Right (tsym, val')
toDefaultParam sp label (Loc.Located _ (SList [name, Loc.Located _ (SType ty), val])) = do
  tsym <- case name of
    Loc.Located _ (SAtom n) -> do
      t <- toType ty
      Right $ CST.TSymbol n (Just t)
    Loc.Located sp' _ -> Left $ ConvertError sp' "invalid parameter name"
  val' <- toExpr val
  Right (tsym, val')
toDefaultParam sp label (Loc.Located sp' _) =
  Left $ ConvertError sp' (label ++ " parameters must be (name default) pairs")

-- LET

toLet :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toLet _ [Loc.Located _ (SList bindings), body] = do
  bindings' <- mapM toBinding bindings
  body'     <- toExpr body
  Right $ CST.ExprLet bindings' body'
toLet sp _ = Left $ ConvertError sp "invalid let: expected (let ((name val)...) body)"

toBinding :: SExpr -> Either ConvertError (CST.TSymbol, CST.Expr)
toBinding (Loc.Located _ (SList [name, val])) = do
  name' <- toTSymbol name
  val'  <- toExpr val
  Right (name', val')
toBinding (Loc.Located sp _) = Left $ ConvertError sp "invalid let binding"

-- IF

toIf :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toIf _ [cond, then', else'] = do
  cond'  <- toExpr cond
  then'' <- toExpr then'
  else'' <- toExpr else'
  Right $ CST.ExprIf cond' then'' else''
toIf sp _ = Left $ ConvertError sp "invalid if: expected (if cond then else)"

-- TYPE

toTypeDecl :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toTypeDecl _ (Loc.Located _ (SAtom name) : Loc.Located _ (SList params) : ctors) = do
  params' <- mapM toAtomName params
  ctors'  <- mapM toDataCon ctors
  Right $ CST.ExprType name params' ctors'
toTypeDecl sp _ = Left $ ConvertError sp "invalid type declaration"

toDataCon :: SExpr -> Either ConvertError CST.DataCon
toDataCon (Loc.Located _ (SList (Loc.Located _ (SAtom name) : args)))
  | not (null args) && all isRecordField args = do
      fields <- mapM toRecordField args
      let (names, types) = unzip fields
      Right $ CST.DataCon name types (Just names)
  | otherwise = do
      args' <- mapM toTypeArg args
      Right $ CST.DataCon name args' Nothing
  where
    isRecordField (Loc.Located _ (SList [Loc.Located _ (SAtom _), tyArg])) =
      isTypeArg tyArg
    isRecordField _ = False
    isTypeArg (Loc.Located _ (SType _)) = True
    isTypeArg (Loc.Located _ (SAtom _)) = True
    isTypeArg _                         = False
    toRecordField (Loc.Located _ (SList [Loc.Located _ (SAtom fname), tyArg])) = do
      ty <- toTypeArg tyArg
      Right (fname, ty)
    toRecordField (Loc.Located sp' _) = Left $ ConvertError sp' "invalid record field"
toDataCon (Loc.Located sp _) = Left $ ConvertError sp "invalid data constructor"

-- CASE

toCase :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toCase _ (scrutinee : arms) = do
  scrutinee' <- toExpr scrutinee
  arms'      <- mapM toArm arms
  Right $ CST.ExprCase scrutinee' arms'
toCase sp [] = Left $ ConvertError sp "invalid case: expected (case scrutinee arms...)"

toArm :: SExpr -> Either ConvertError (CST.Pattern, CST.Expr)
toArm (Loc.Located _ (SList [pat, body])) = do
  pat'  <- toPattern pat
  body' <- toExpr body
  Right (pat', body')
toArm (Loc.Located sp _) = Left $ ConvertError sp "invalid case arm"

-- TYPECLASSES

toCls :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toCls sp (Loc.Located _ (SAtom name) : Loc.Located _ (SList tvars) : methods)
  | null tvars = Left $ ConvertError sp "cls requires at least one type variable"
  | null methods = Left $ ConvertError sp "cls requires at least one method"
  | otherwise = do
      tvars' <- mapM toAtomName tvars
      methods' <- mapM (toClassMethod sp) methods
      Right $ CST.ExprCls name tvars' methods'
toCls sp _ = Left $ ConvertError sp "invalid cls: expected (cls Name (tyvars...) methods...)"

toClassMethod :: Loc.Span -> SExpr -> Either ConvertError CST.ClassMethod
toClassMethod sp (Loc.Located _ (SList (Loc.Located _ (SAtom name) : tys)))
  | length tys < 2 = Left $ ConvertError sp "class method must have at least one arg type and a return type"
  | otherwise = do
      tys' <- mapM toMethodType tys
      let argTys = init tys'
          retTy  = last tys'
      Right $ CST.ClassMethod name argTys retTy
toClassMethod sp _ = Left $ ConvertError sp "invalid class method signature"

toMethodType :: SExpr -> Either ConvertError Ty.Type
toMethodType (Loc.Located _ (SType inner)) = toType inner
toMethodType (Loc.Located _ (SAtom name)) = Right (Ty.TyCon name [])
toMethodType (Loc.Located sp _) = Left $ ConvertError sp "invalid type in method signature"

toInst :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toInst sp (Loc.Located _ (SAtom className) : Loc.Located _ (SType tyExpr) : methods)
  | null methods = Left $ ConvertError sp "inst requires at least one method"
  | otherwise = do
      ty <- toType tyExpr
      methods' <- mapM toInstMethod methods
      Right $ CST.ExprInst className ty methods'
toInst sp _ = Left $ ConvertError sp "invalid inst: expected (inst ClassName %Type methods...)"

toInstMethod :: SExpr -> Either ConvertError (CST.Symbol, CST.Expr)
toInstMethod (Loc.Located _ (SList [Loc.Located _ (SAtom name), body])) = do
  body' <- toExpr body
  Right (name, body')
toInstMethod (Loc.Located sp _) = Left $ ConvertError sp "invalid instance method: expected (name expr)"

-- FFI

toFFIDecl :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toFFIDecl _ [Loc.Located _ (SAtom name), Loc.Located _ (SList paramTyExprs), retTyExpr] = do
  paramTys <- mapM toCTypeArg paramTyExprs
  retTy <- toCTypeArg retTyExpr
  Right $ CST.ExprFFI name paramTys retTy
toFFIDecl sp _ = Left $ ConvertError sp "invalid ffi: expected (ffi name (param-types...) return-type)"

toFFIStruct :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toFFIStruct _ (Loc.Located _ (SAtom name) : fields)
  | not (null fields) = do
      fs <- mapM toStructField fields
      Right $ CST.ExprFFIStruct name fs
toFFIStruct sp _ = Left $ ConvertError sp "invalid ffi-struct: expected (ffi-struct Name (field %CType)...)"

toStructField :: SExpr -> Either ConvertError (CST.Symbol, Ty.CType)
toStructField (Loc.Located _ (SList [Loc.Located _ (SAtom fname), tyExpr])) = do
  ct <- toCTypeArg tyExpr
  Right (fname, ct)
toStructField (Loc.Located sp _) = Left $ ConvertError sp "invalid struct field: expected (name %CType)"

toFFIVar :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toFFIVar _ [Loc.Located _ (SAtom name), Loc.Located _ (SList paramTyExprs), retTyExpr] = do
  paramTys <- mapM toCTypeArg paramTyExprs
  retTy <- toCTypeArg retTyExpr
  Right $ CST.ExprFFIVar name paramTys retTy
toFFIVar sp _ = Left $ ConvertError sp "invalid ffi-var: expected (ffi-var name (fixed-params...) return-type)"

toFFIEnum :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toFFIEnum _ (Loc.Located _ (SAtom name) : variants)
  | not (null variants) = do
      vs <- mapM toEnumVariant variants
      Right $ CST.ExprFFIEnum name vs
toFFIEnum sp _ = Left $ ConvertError sp "invalid ffi-enum: expected (ffi-enum Name (Variant value)...)"

toEnumVariant :: SExpr -> Either ConvertError (CST.Symbol, Integer)
toEnumVariant (Loc.Located _ (SList [Loc.Located _ (SAtom name), Loc.Located _ (SInt val)])) =
  Right (name, val)
toEnumVariant (Loc.Located sp _) = Left $ ConvertError sp "invalid enum variant: expected (NAME integer)"

toFFICallback :: Loc.Span -> [SExpr] -> Either ConvertError CST.ExprF
toFFICallback _ [Loc.Located _ (SAtom name), Loc.Located _ (SList paramTyExprs), retTyExpr] = do
  paramTys <- mapM toCTypeArg paramTyExprs
  retTy <- toCTypeArg retTyExpr
  Right $ CST.ExprFFICallback name paramTys retTy
toFFICallback sp _ = Left $ ConvertError sp "invalid ffi-callback: expected (ffi-callback name (param-types...) return-type)"

toCTypeArg :: SExpr -> Either ConvertError Ty.CType
toCTypeArg (Loc.Located _ (SType inner)) = toCType inner
-- (%ARR N %ElemType) is parsed as a list: [%ARR, N, %ElemType]
toCTypeArg (Loc.Located _ (SList (Loc.Located _ (SType (Loc.Located _ (SAtom "ARR"))) : rest))) =
  toArrType rest
toCTypeArg (Loc.Located sp _) = Left $ ConvertError sp "invalid C type in ffi declaration"

toArrType :: [SExpr] -> Either ConvertError Ty.CType
toArrType [Loc.Located _ (SInt n), elemExpr] = do
  elemTy <- toCTypeArg elemExpr
  Right $ Ty.CArr (fromIntegral n) elemTy
toArrType _ = Left $ ConvertError dummySpan "invalid array type: expected (%ARR count %ElemType)"

toCType :: SExpr -> Either ConvertError Ty.CType
toCType (Loc.Located _ (SAtom name)) = case name of
  -- Backward-compatible aliases: existing pllisp types map to native-size C types
  "INT"  -> Right Ty.CI64
  "FLT"  -> Right Ty.CF64
  "STR"  -> Right Ty.CPtr
  "UNIT" -> Right Ty.CVoid
  -- Explicit C type names
  "I8"   -> Right Ty.CI8
  "I16"  -> Right Ty.CI16
  "I32"  -> Right Ty.CI32
  "I64"  -> Right Ty.CI64
  "U8"   -> Right Ty.CU8
  "U16"  -> Right Ty.CU16
  "U32"  -> Right Ty.CU32
  "U64"  -> Right Ty.CU64
  "F32"  -> Right Ty.CF32
  "F64"  -> Right Ty.CF64
  "PTR"  -> Right Ty.CPtr
  "VOID" -> Right Ty.CVoid
  other  -> Right $ Ty.CStruct other  -- struct name reference for nested structs
toCType (Loc.Located sp _) = Left $ ConvertError sp "invalid C type in ffi declaration"

-- PATTERNS

toPattern :: SExpr -> Either ConvertError CST.Pattern
toPattern (Loc.Located _ sexpr) = case sexpr of
  SAtom "_"     -> Right CST.PatWild
  SAtom "TRUE"  -> Right (CST.PatBool True)
  SAtom "FALSE" -> Right (CST.PatBool False)
  SAtom name    -> Right (CST.PatVar name)
  SInt n        -> Right (CST.PatLit (CST.LitInt n))
  SFlt d        -> Right (CST.PatLit (CST.LitFlt d))
  SStr s        -> Right (CST.PatLit (CST.LitStr s))
  SRx p f       -> Right (CST.PatLit (CST.LitRx p f))
  SList (Loc.Located _ (SAtom name) : args) -> do
    args' <- mapM toPattern args
    Right (CST.PatCon name args')
  other         -> Left $ ConvertError (dummySpan) ("invalid pattern: " ++ show other)

-- TYPED SYMBOLS

toTSymbol :: SExpr -> Either ConvertError CST.TSymbol
toTSymbol (Loc.Located _ (SAtom name)) =
  Right $ CST.TSymbol name Nothing
toTSymbol (Loc.Located _ (SList [Loc.Located _ (SAtom name), Loc.Located _ (SType tyExpr)])) = do
  ty <- toType tyExpr
  Right $ CST.TSymbol name (Just ty)
toTSymbol (Loc.Located sp _) = Left $ ConvertError sp "invalid typed symbol"

-- TYPES

toType :: SExpr -> Either ConvertError Ty.Type
toType (Loc.Located _ (SAtom name)) = case name of
  "INT"  -> Right Ty.TyInt
  "FLT"  -> Right Ty.TyFlt
  "STR"  -> Right Ty.TyStr
  "BOOL" -> Right Ty.TyBool
  "UNIT" -> Right Ty.TyUnit
  "RX"   -> Right Ty.TyRx
  other  -> Right (Ty.TyCon other [])
toType (Loc.Located _ (SList (Loc.Located _ (SAtom name) : args))) = do
  args' <- mapM toTypeArg args
  Right (Ty.TyCon name args')
toType (Loc.Located sp _) = Left $ ConvertError sp "invalid type"

toTypeArg :: SExpr -> Either ConvertError Ty.Type
toTypeArg (Loc.Located _ (SType inner)) = toType inner
toTypeArg (Loc.Located _ (SAtom name))  = Right (Ty.TyCon name [])
toTypeArg (Loc.Located sp _) = Left $ ConvertError sp "invalid type argument"

-- KEYWORD ARGS AT CALL SITES

-- | Check if an SExpr is a &KEY marker in an argument list.
isKeyArg :: SExpr -> Bool
isKeyArg (Loc.Located _ (SAtom "&KEY")) = True
isKeyArg _ = False

-- | Process an argument list containing &KEY markers into positional + KeyArg exprs.
processKeyArgs :: Loc.Span -> [SExpr] -> Either ConvertError [CST.Expr]
processKeyArgs _ [] = Right []
processKeyArgs sp (Loc.Located _ (SAtom "&KEY") : Loc.Located _ (SAtom name) : val : rest) = do
  val' <- toExpr val
  let keyArg = Loc.Located sp (CST.ExprKeyArg name val')
  rest' <- processKeyArgs sp rest
  Right (keyArg : rest')
processKeyArgs sp (Loc.Located sp' (SAtom "&KEY") : _) =
  Left $ ConvertError sp' "&key must be followed by a name and value"
processKeyArgs sp (arg : rest) = do
  arg' <- toExpr arg
  rest' <- processKeyArgs sp rest
  Right (arg' : rest')

-- HELPERS

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
