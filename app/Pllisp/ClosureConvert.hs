{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.ClosureConvert where

import qualified Pllisp.BuiltIn as BuiltIn
import qualified Pllisp.CST as CST
import qualified Pllisp.Resolve as Res
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ENTRY POINT

closureConvert :: TC.TResolvedCST -> CCProgram
closureConvert = map convertExpr

-- CORE

type CCProgram = [CCExpr]
type CCExpr    = Ty.Typed CCExprF

-- CCExprF mirrors TRExprF from TypeCheck (which itself mirrors RExprF from
-- Resolve), except CCLam gains an explicit free variable list and CCVar drops
-- VarBinding in favour of a plain symbol since scope indices are no longer needed.
data CCExprF
  = CCLam  [(CST.Symbol, Ty.Type)]  -- params
           [(CST.Symbol, Ty.Type)]  -- captured free vars (env struct contents)
           Ty.Type                  -- return type
           CCExpr                   -- body
  | CCLit  CST.Literal
  | CCBool Bool
  | CCUnit
  | CCVar  CST.Symbol Ty.Type
  | CCLet  [(CST.Symbol, Ty.Type, CCExpr)] CCExpr
  | CCIf   CCExpr CCExpr CCExpr
  | CCApp  CCExpr [CCExpr]
  | CCType CST.Symbol [CST.Symbol] [CST.DataCon]
  | CCCase CCExpr [(CCPattern, CCExpr)]
  deriving (Eq, Show)

data CCPattern
  = CCPatLit  CST.Literal
  | CCPatBool Bool
  | CCPatVar  CST.Symbol Ty.Type
  | CCPatWild Ty.Type
  | CCPatCon  CST.Symbol Ty.Type [CCPattern]
  deriving (Eq, Show)

-- CONVERSION

-- | Convert a single top-level typed expression, stripping the Located wrapper.
convertExpr :: TC.TRExpr -> CCExpr
convertExpr (Loc.Located _ (Ty.Typed t expr)) = Ty.Typed t $ case expr of
  TC.TRLit l    -> CCLit l
  TC.TRBool b   -> CCBool b
  TC.TRUnit     -> CCUnit
  TC.TRVar vb   -> CCVar (Res.symName vb) t
  TC.TRIf c t e -> CCIf (convertExpr c) (convertExpr t) (convertExpr e)
  TC.TRApp f as -> CCApp (convertExpr f) (map convertExpr as)
  TC.TRLet binds body ->
    let ccBinds = [(n, bt, convertExpr rhs) | (n, bt, rhs) <- binds]
    in CCLet ccBinds (convertExpr body)
  TC.TRType n ps cs -> CCType n ps cs
  TC.TRCase scr arms ->
    CCCase (convertExpr scr) [(convertPattern p, convertExpr b) | (p, b) <- arms]
  TC.TRLam params retTy body ->
    -- This is the interesting case: compute free variables of the lambda body,
    -- subtract the params (and built-ins/constructors), and attach them.
    let bodyFvMap  = freeVars body
        paramNames = S.fromList (map fst params)
        free       = M.withoutKeys bodyFvMap (S.union paramNames globalNames)
    in CCLam params (M.toList free) retTy (convertExpr body)

convertPattern :: TC.TRPattern -> CCPattern
convertPattern pat = case pat of
  TC.TRPatLit l      -> CCPatLit l
  TC.TRPatBool b     -> CCPatBool b
  TC.TRPatVar s t    -> CCPatVar s t
  TC.TRPatWild t     -> CCPatWild t
  TC.TRPatCon c t ps -> CCPatCon c t (map convertPattern ps)

-- FREE VARIABLES
--
-- Compute the set of free (symbol, type) pairs in a typed expression.
-- Built-in names should be excluded.

-- | Names that should never appear as captured free variables.
globalNames :: S.Set CST.Symbol
globalNames = BuiltIn.builtInNames

-- | Free variables of a TRExpr. Returns a map from symbol to its type.
freeVars :: TC.TRExpr -> M.Map CST.Symbol Ty.Type
freeVars (Loc.Located _ (Ty.Typed t expr)) = case expr of
  TC.TRVar v -> M.singleton (Res.symName v) t
  TC.TRLam params _ body ->
    M.withoutKeys (freeVars body) (S.fromList (map fst params))
  TC.TRLet binds body ->
    let bindFvs  = M.unions [freeVars rhs | (_, _, rhs) <- binds]
        bodyFvs  = M.withoutKeys (freeVars body) (S.fromList [n | (n, _, _) <- binds])
    in M.union bindFvs bodyFvs
  TC.TRIf c th el -> M.unions [freeVars c, freeVars th, freeVars el]
  TC.TRApp f as   -> M.unions (freeVars f : map freeVars as)
  TC.TRCase scr arms ->
    M.union (freeVars scr) $ M.unions
      [M.withoutKeys (freeVars b) (patternBinds p) | (p, b) <- arms]
  _ -> M.empty

-- | Free variables of a pattern (the variables it binds, which should be
-- subtracted from the arm body's free vars).
patternBinds :: TC.TRPattern -> S.Set CST.Symbol
patternBinds (TC.TRPatVar s _)    = S.singleton s
patternBinds (TC.TRPatCon _ _ ps) = S.unions (map patternBinds ps)
patternBinds _                    = S.empty
