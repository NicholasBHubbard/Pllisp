module Main (main) where

import Test.Hspec

import qualified BuiltInSpec
import qualified ErrorSpec
import qualified ExhaustCheckSpec
import qualified ParserSpec
import qualified PropSpec
import qualified ResolveSpec
import qualified TypeCheckSpec
import qualified TypeSpec
import qualified ClosureConvertSpec
import qualified CodegenSpec
import qualified LambdaLiftSpec
import qualified MacroExpandSpec
import qualified MacroInterpSpec
import qualified ReplSpec
import qualified ModuleSpec
import qualified SExprSpec

main :: IO ()
main = hspec $ do
  describe "Type"      TypeSpec.spec
  describe "BuiltIn"   BuiltInSpec.spec
  describe "Parser"    ParserSpec.spec
  describe "SExpr"     SExprSpec.spec
  describe "Resolve"   ResolveSpec.spec
  describe "TypeCheck" TypeCheckSpec.spec
  describe "Exhaust"   ExhaustCheckSpec.spec
  describe "Error"     ErrorSpec.spec
  describe "Props"     PropSpec.spec
  describe "ClosureConvert" ClosureConvertSpec.spec
  describe "LambdaLift"    LambdaLiftSpec.spec
  describe "Module"         ModuleSpec.spec
  describe "Codegen"        CodegenSpec.spec
  describe "MacroExpand"    MacroExpandSpec.spec
  describe "MacroInterp"   MacroInterpSpec.spec
  describe "Repl"           ReplSpec.spec
