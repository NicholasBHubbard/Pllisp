{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.BuiltIn where

import qualified Pllisp.Type as Ty

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

-- CORE

type Symbol = T.Text

-- For Resolve: names that are always in scope
builtInNames :: S.Set Symbol
builtInNames = S.union (M.keysSet builtInTypes) (M.keysSet polyBuiltIns)

-- For TypeCheck: types of built-in functions (monomorphic, so empty forall)
builtInSchemes :: M.Map Symbol (S.Set Integer, Ty.Type)
builtInSchemes = M.union polyBuiltIns (M.map (\t -> (S.empty, t)) builtInTypes)

-- Polymorphic built-ins (quantified over type variable 0)
polyBuiltIns :: M.Map Symbol (S.Set Integer, Ty.Type)
polyBuiltIns = M.fromList
  [ ("REF",   (S.singleton 0, Ty.TyFun [Ty.TyVar 0] (Ty.TyCon "REF" [Ty.TyVar 0])))
  , ("DEREF", (S.singleton 0, Ty.TyFun [Ty.TyCon "REF" [Ty.TyVar 0]] (Ty.TyVar 0)))
  , ("SET!",  (S.singleton 0, Ty.TyFun [Ty.TyCon "REF" [Ty.TyVar 0], Ty.TyVar 0] Ty.TyUnit))
  ]

builtInTypes :: M.Map Symbol Ty.Type
builtInTypes = M.fromList
  -- Arithmetic (Int)
  [ ("ADD", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("SUB", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("MUL", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("DIV", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("MOD", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  -- Arithmetic (Float)
  , ("ADDF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("SUBF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("MULF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("DIVF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  -- Comparison (Int)
  , ("EQI", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  , ("LTI", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  -- Comparison (Float)
  , ("EQF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyBool)
  , ("LTF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyBool)
  -- Comparison (String)
  , ("EQS", Ty.TyFun [Ty.TyStr, Ty.TyStr] Ty.TyBool)
  , ("LTS", Ty.TyFun [Ty.TyStr, Ty.TyStr] Ty.TyBool)
  -- String
  , ("CONCAT", Ty.TyFun [Ty.TyStr, Ty.TyStr] Ty.TyStr)
  , ("STRLEN", Ty.TyFun [Ty.TyStr] Ty.TyInt)
  , ("SUBSTR", Ty.TyFun [Ty.TyStr, Ty.TyInt, Ty.TyInt] Ty.TyStr)
  -- Conversion
  , ("INT-TO-FLT", Ty.TyFun [Ty.TyInt] Ty.TyFlt)
  , ("FLT-TO-INT", Ty.TyFun [Ty.TyFlt] Ty.TyInt)
  , ("USYM-TO-STR", Ty.TyFun [Ty.TyUSym] Ty.TyStr)
  , ("STR-TO-USYM", Ty.TyFun [Ty.TyStr] Ty.TyUSym)
  ]
