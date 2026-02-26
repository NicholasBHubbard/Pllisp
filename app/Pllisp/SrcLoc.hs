-- MODULE

module Pllisp.SrcLoc where

-- Core

data Located a = Located
  { locSpan :: Span
  , locVal  :: a
  } deriving (Eq, Show)

data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  } deriving (Eq, Show)

data Pos = Pos
  { posFile  :: FilePath
  , posLinum :: Int -- 1 based
  , posCol   :: Int -- 1 based
  } deriving (Eq, Show)
