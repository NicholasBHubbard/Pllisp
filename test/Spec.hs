module Main (main) where

import Test.Hspec

import qualified BuiltInSpec
import qualified ErrorSpec
import qualified ExhaustSpec
import qualified ParserSpec
import qualified PropSpec
import qualified ResolveSpec
import qualified TypeCheckSpec
import qualified TypeSpec

main :: IO ()
main = hspec $ do
  describe "Type"      TypeSpec.spec
  describe "BuiltIn"   BuiltInSpec.spec
  describe "Parser"    ParserSpec.spec
  describe "Resolve"   ResolveSpec.spec
  describe "TypeCheck" TypeCheckSpec.spec
  describe "Exhaust"   ExhaustSpec.spec
  describe "Error"     ErrorSpec.spec
  describe "Props"     PropSpec.spec
