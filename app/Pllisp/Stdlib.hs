module Pllisp.Stdlib (loadPrelude) where

import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.Parser as Parser

import qualified Data.Text.IO as T.IO
import Paths_pllisp (getDataFileName)

loadPrelude :: IO [SExpr.SExpr]
loadPrelude = do
  fp <- getDataFileName "PRELUDE.pll"
  src <- T.IO.readFile fp
  case Parser.parseSExprs "<prelude>" src of
    Left _    -> error "BUG: prelude failed to parse"
    Right sexprs -> pure sexprs
