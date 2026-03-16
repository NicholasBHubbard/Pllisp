{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Type where

import qualified Data.Text as T

-- CORE

type Symbol = T.Text

data Typed a = Typed
  { ty  :: Type
  , val :: a
  } deriving (Eq, Show)

data Type
  = TyInt
  | TyFlt
  | TyStr
  | TyBool
  | TyUnit
  | TyFun [Type] Type
  | TyCon Symbol [Type]  -- user-defined: TyCon "Maybe" [TyInt]
  | TyVar Integer
  deriving (Eq, Show)

renderType :: Type -> T.Text
renderType t = case t of
  TyInt      -> "%INT"
  TyFlt      -> "%FLT"
  TyStr      -> "%STR"
  TyBool     -> "%BOOL"
  TyUnit     -> "%UNIT"
  TyFun as r -> "%(" <> T.intercalate " " (map renderType as) <> " -> " <> renderType r <> ")"
  TyCon s [] -> "%" <> s
  TyCon s ts -> "%(" <> s <> " " <> T.intercalate " " (map renderType ts) <> ")"
  TyVar n    -> "%t" <> T.pack (show n)
