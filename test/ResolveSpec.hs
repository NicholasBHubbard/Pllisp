{-# LANGUAGE OverloadedStrings #-}

module ResolveSpec (spec) where

import Test.Hspec

import qualified Data.Text as T

import qualified Pllisp.Parser  as Parser
import qualified Pllisp.Resolve as Resolve

spec :: Spec
spec = do
  describe "error collection" $ do
    it "collects multiple undefined symbol errors" $ do
      case parseAndResolve "(add x y)" of
        Right _ -> expectationFailure "expected resolve errors"
        Left errs -> length errs `shouldBe` 2

    it "collects duplicate lambda parameter errors" $ do
      case parseAndResolve "(lam (x x) x)" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "collects errors from nested scopes" $ do
      case parseAndResolve "(lam (x) y)" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldBe` 1

    it "collects duplicate let binding errors" $ do
      case parseAndResolve "(let ((x 1) (x 2)) x)" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "collects duplicate type parameter errors" $ do
      case parseAndResolve "(type Foo (a a) (Bar))" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "collects duplicate data constructor errors" $ do
      case parseAndResolve "(type Foo () (Bar) (Bar))" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "collects duplicate pattern variable errors" $ do
      case parseAndResolve "(type Pair () (Mk %INT %INT)) (let ((x (Mk 1 2))) (case x ((Mk y y) 0)))" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "unknown constructor in pattern" $ do
      case parseAndResolve "(let ((x 1)) (case x ((BadCtor) 0)))" of
        Right _ -> expectationFailure "expected resolve error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

  describe "valid programs" $ do
    it "resolves simple let bindings" $ do
      parseAndResolve "(let ((x 1)) x)" `shouldSatisfy` isRight

    it "resolves lambda parameters" $ do
      parseAndResolve "(lam (x) x)" `shouldSatisfy` isRight

    it "resolves nested scope shadowing" $ do
      parseAndResolve "(let ((x 1)) (let ((x 2)) x))" `shouldSatisfy` isRight

    it "resolves built-ins in scope" $ do
      parseAndResolve "(add 1 2)" `shouldSatisfy` isRight

    it "resolves constructor defined in same program" $ do
      parseAndResolve "(type M (a) (J a)) (let ((v (J 1))) v)" `shouldSatisfy` isRight

    it "resolves case with bound variable" $ do
      parseAndResolve "(type M (a) (J a)) (let ((v (J 1))) (case v ((J y) y)))"
        `shouldSatisfy` isRight

    it "resolves RLit" $ do
      parseAndResolve "42" `shouldSatisfy` isRight

    it "resolves RBool" $ do
      parseAndResolve "true" `shouldSatisfy` isRight

    it "resolves RIf" $ do
      parseAndResolve "(if true 1 2)" `shouldSatisfy` isRight

    it "resolves RApp" $ do
      parseAndResolve "(add 1 2)" `shouldSatisfy` isRight

    it "resolves RType" $ do
      parseAndResolve "(type Foo () (Bar))" `shouldSatisfy` isRight

-- Helpers

parseAndResolve :: T.Text -> Either [Resolve.ResolveError] Resolve.ResolvedCST
parseAndResolve src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right cst -> Resolve.resolve cst

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False
