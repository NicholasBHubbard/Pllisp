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
builtInNames = M.keysSet builtInTypes

-- For TypeCheck: types of built-in functions (monomorphic, so empty forall)
builtInSchemes :: M.Map Symbol (S.Set Integer, Ty.Type)
builtInSchemes = M.map (\t -> (S.empty, t)) builtInTypes

builtInTypes :: M.Map Symbol Ty.Type
builtInTypes = M.fromList
  -- Arithmetic (Int)
  [ ("ADD", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("SUB", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("MUL", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("DIV", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("MOD", Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)
  , ("NEG", Ty.TyFun [Ty.TyInt] Ty.TyInt)
  -- Arithmetic (Float)
  , ("ADDF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("SUBF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("MULF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  , ("DIVF", Ty.TyFun [Ty.TyFlt, Ty.TyFlt] Ty.TyFlt)
  -- Comparison
  , ("EQ",  Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  , ("LT",  Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  , ("GT",  Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  , ("LE",  Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  , ("GE",  Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyBool)
  -- Boolean
  , ("AND", Ty.TyFun [Ty.TyBool, Ty.TyBool] Ty.TyBool)
  , ("OR",  Ty.TyFun [Ty.TyBool, Ty.TyBool] Ty.TyBool)
  , ("NOT", Ty.TyFun [Ty.TyBool] Ty.TyBool)
  -- String
  , ("CONCAT", Ty.TyFun [Ty.TyStr, Ty.TyStr] Ty.TyStr)
  , ("STRLEN", Ty.TyFun [Ty.TyStr] Ty.TyInt)
  -- IO
  , ("PRINT", Ty.TyFun [Ty.TyStr] Ty.TyUnit)
  -- Conversion
  , ("INT-TO-FLT", Ty.TyFun [Ty.TyInt] Ty.TyFlt)
  , ("FLT-TO-INT", Ty.TyFun [Ty.TyFlt] Ty.TyInt)
  , ("INT-TO-STR", Ty.TyFun [Ty.TyInt] Ty.TyStr)
  , ("FLT-TO-STR", Ty.TyFun [Ty.TyFlt] Ty.TyStr)
  ]
