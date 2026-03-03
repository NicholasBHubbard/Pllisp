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
  | TyFun [Type] Type
  | TyCon Symbol         -- for user-defined types later
  | TyVar Integer
  deriving (Eq, Show)

renderType :: Type -> T.Text
renderType t = case t of
  TyInt      -> "%INT"
  TyFlt      -> "%FLT"
  TyStr      -> "%STR"
  TyBool     -> "%BOOL"
  TyFun as r -> "%(" <> T.intercalate " " (map renderType as) <> " -> " <> renderType r <> ")"
  TyCon s    -> "%" <> s
  TyVar n    -> "%t" <> T.pack (show n)
