-- MODULE

module Pllisp.TypeCheck where

import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Resolve as Res
import qualified Pllisp.Type as Ty

import qualified Data.Map.Strict as M

-- ENTRY POINT

typecheck :: Res.ResolvedCST -> Either TypeError TResolvedCST
typecheck exprs =
  case runInfer (traverse inferExpr exprs) M.empty M.empty 0 of
    Left err -> Left err
    Right (texprs, subst, _) -> Right (map (applySubstExpr subst) texprs)

-- CORE

type TResolvedCST = [TRExpr]

type TRExpr = Loc.Located (Ty.Typed TRExprF)

data TRExprF
  = TRLit  CST.Literal
  | TRBool Bool
  | TRVar  Res.VarBinding
  | TRLam  [(CST.Symbol, Ty.Type)] Ty.Type TRExpr
  | TRLet  [(CST.Symbol, Ty.Type, TRExpr)] TRExpr
  | TRIf   TRExpr TRExpr TRExpr
  | TRApp  TRExpr [TRExpr]
  deriving (Eq, Show)

data TypeError = TypeError
  { teSpan :: Loc.Span
  , teMsg  :: String
  }

type Subst = M.Map Integer Ty.Type

type TypeEnv = M.Map Res.VarBinding Ty.Type

newtype Infer a = Infer
  { runInfer :: TypeEnv -> Subst -> Integer -> Either TypeError (a, Subst, Integer)
  }

instance Functor Infer where
  fmap f (Infer m) = Infer $ \env s n ->
    case m env s n of
      Left err -> Left err
      Right (a, s', n') -> Right (f a, s', n')

instance Applicative Infer where
  pure a = Infer $ \_ s n -> Right (a, s, n)
  Infer mf <*> Infer ma = Infer $ \env s n ->
    case mf env s n of
      Left err -> Left err
      Right (f, s', n') ->
        case ma env s' n' of
          Left err -> Left err
          Right (a, s'', n'') -> Right (f a, s'', n'')

instance Monad Infer where
  Infer ma >>= f = Infer $ \env s n ->
    case ma env s n of
      Left err -> Left err
      Right (a, s', n') -> runInfer (f a) env s' n'

fresh :: Infer Ty.Type
fresh = Infer $ \_ s n -> Right (Ty.TyVar n, s, n + 1)

throwError :: Loc.Span -> String -> Infer a
throwError sp msg = Infer $ \_ _ _ -> Left (TypeError sp msg)

lookupVar :: Loc.Span -> Res.VarBinding -> Infer Ty.Type
lookupVar sp vb = Infer $ \env s n ->
  case M.lookup vb env of
    Just t  -> Right (t, s, n)
    Nothing -> Left (TypeError sp ("unbound variable: " ++ show (Res.symName vb)))

withBindings :: [(Res.VarBinding, Ty.Type)] -> Infer a -> Infer a
withBindings binds (Infer m) = Infer $ \env s n ->
  let env' = foldr (uncurry M.insert) env binds
  in m env' s n

getSubst :: Infer Subst
getSubst = Infer $ \_ s n -> Right (s, s, n)

extendSubst :: Integer -> Ty.Type -> Infer ()
extendSubst var t = Infer $ \_ s n -> Right ((), M.insert var t s, n)

-- SUBSTITUTION

applySubst :: Subst -> Ty.Type -> Ty.Type
applySubst s t = case t of
  Ty.TyVar n    -> maybe t (applySubst s) (M.lookup n s)
  Ty.TyFun as r -> Ty.TyFun (map (applySubst s) as) (applySubst s r)
  _             -> t

-- UNIFICATION

occursIn :: Integer -> Ty.Type -> Bool
occursIn n t = case t of
  Ty.TyVar m    -> n == m
  Ty.TyFun as r -> any (occursIn n) as || occursIn n r
  _             -> False

unify :: Loc.Span -> Ty.Type -> Ty.Type -> Infer ()
unify sp t1 t2 = do
  s <- getSubst
  let t1' = applySubst s t1
      t2' = applySubst s t2
  unify' sp t1' t2'

unify' :: Loc.Span -> Ty.Type -> Ty.Type -> Infer ()
unify' sp t1 t2 = case (t1, t2) of
  (Ty.TyInt, Ty.TyInt)   -> pure ()
  (Ty.TyFlt, Ty.TyFlt)   -> pure ()
  (Ty.TyStr, Ty.TyStr)   -> pure ()
  (Ty.TyBool, Ty.TyBool) -> pure ()
  (Ty.TyCon a, Ty.TyCon b) | a == b -> pure ()
  (Ty.TyVar n, t) -> bindVar sp n t
  (t, Ty.TyVar n) -> bindVar sp n t
  (Ty.TyFun as1 r1, Ty.TyFun as2 r2)
    | length as1 == length as2 -> do
        zipWithM_ (unify sp) as1 as2
        unify sp r1 r2
  _ -> throwError sp $ "cannot unify " ++ show t1 ++ " with " ++ show t2

bindVar :: Loc.Span -> Integer -> Ty.Type -> Infer ()
bindVar sp n t
  | Ty.TyVar n == t = pure ()
  | occursIn n t    = throwError sp "infinite type"
  | otherwise       = extendSubst n t

zipWithM_ :: (a -> b -> Infer ()) -> [a] -> [b] -> Infer ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-- INFERENCE

inferExpr :: Res.RExpr -> Infer TRExpr
inferExpr (Loc.Located sp expr) = case expr of

  Res.RLit lit -> do
    let t = case lit of
          CST.LitInt _ -> Ty.TyInt
          CST.LitFlt _ -> Ty.TyFlt
          CST.LitStr _ -> Ty.TyStr
    pure $ Loc.Located sp (Ty.Typed t (TRLit lit))

  Res.RBool b ->
    pure $ Loc.Located sp (Ty.Typed Ty.TyBool (TRBool b))

  Res.RVar vb -> do
    t <- lookupVar sp vb
    pure $ Loc.Located sp (Ty.Typed t (TRVar vb))

  Res.RLam params mRetTy body -> do
    paramTypes <- traverse (\p -> maybe fresh pure (CST.symType p)) params
    let bindings = zipWith (\p t -> (Res.VarBinding 0 (CST.symName p), t)) params paramTypes
    tbody <- withBindings bindings (inferExpr body)
    let bodyTy = Ty.ty (Loc.locVal tbody)
    case mRetTy of
      Just retTy -> unify sp bodyTy retTy
      Nothing    -> pure ()
    let retTy = maybe bodyTy id mRetTy
        funTy = Ty.TyFun paramTypes retTy
        paramList = zip (map CST.symName params) paramTypes
    pure $ Loc.Located sp (Ty.Typed funTy (TRLam paramList retTy tbody))

  Res.RLet binds body -> do
    (tbinds, bindingPairs) <- inferLetBinds sp binds
    tbody <- withBindings bindingPairs (inferExpr body)
    let bodyTy = Ty.ty (Loc.locVal tbody)
        typedBinds = [(CST.symName (fst b), Ty.ty (Loc.locVal tb), tb) | (b, tb) <- zip binds tbinds]
    pure $ Loc.Located sp (Ty.Typed bodyTy (TRLet typedBinds tbody))

  Res.RIf cond thn els -> do
    tcond <- inferExpr cond
    tthn  <- inferExpr thn
    tels  <- inferExpr els
    unify sp (Ty.ty (Loc.locVal tcond)) Ty.TyBool
    unify sp (Ty.ty (Loc.locVal tthn)) (Ty.ty (Loc.locVal tels))
    let resTy = Ty.ty (Loc.locVal tthn)
    pure $ Loc.Located sp (Ty.Typed resTy (TRIf tcond tthn tels))

  Res.RApp fun args -> do
    tfun  <- inferExpr fun
    targs <- traverse inferExpr args
    retTy <- fresh
    let funTy  = Ty.ty (Loc.locVal tfun)
        argTys = map (Ty.ty . Loc.locVal) targs
    unify sp funTy (Ty.TyFun argTys retTy)
    pure $ Loc.Located sp (Ty.Typed retTy (TRApp tfun targs))

inferLetBinds :: Loc.Span -> [(CST.TSymbol, Res.RExpr)] -> Infer ([TRExpr], [(Res.VarBinding, Ty.Type)])
inferLetBinds sp binds = do
  tbinds <- traverse inferBind binds
  let bindingPairs = [(Res.VarBinding 0 (CST.symName sym), Ty.ty (Loc.locVal trhs)) | ((sym, _), trhs) <- zip binds tbinds]
  pure (tbinds, bindingPairs)
  where
    inferBind (sym, rhs) = do
      trhs <- inferExpr rhs
      case CST.symType sym of
        Just annTy -> unify sp (Ty.ty (Loc.locVal trhs)) annTy
        Nothing    -> pure ()
      pure trhs

-- FINALIZATION

applySubstExpr :: Subst -> TRExpr -> TRExpr
applySubstExpr s (Loc.Located sp (Ty.Typed t expr)) =
  Loc.Located sp (Ty.Typed (applySubst s t) (applySubstExprF s expr))

applySubstExprF :: Subst -> TRExprF -> TRExprF
applySubstExprF s expr = case expr of
  TRLit l      -> TRLit l
  TRBool b     -> TRBool b
  TRVar vb     -> TRVar vb
  TRLam ps r b -> TRLam [(n, applySubst s t) | (n,t) <- ps] (applySubst s r) (applySubstExpr s b)
  TRLet bs b   -> TRLet [(n, applySubst s t, applySubstExpr s e) | (n,t,e) <- bs] (applySubstExpr s b)
  TRIf c t e   -> TRIf (applySubstExpr s c) (applySubstExpr s t) (applySubstExpr s e)
  TRApp f args -> TRApp (applySubstExpr s f) (map (applySubstExpr s) args)
