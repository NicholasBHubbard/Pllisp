module Main (main) where

import Test.Hspec

import qualified ResolveSpec
import qualified TypeCheckSpec

main :: IO ()
main = hspec $ do
  describe "Resolve" ResolveSpec.spec
  describe "TypeCheck" TypeCheckSpec.spec
