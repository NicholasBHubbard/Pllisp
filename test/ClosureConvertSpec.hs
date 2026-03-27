{-# LANGUAGE OverloadedStrings #-}

module ClosureConvertSpec (spec) where

import Test.Hspec

import qualified Data.List       as L
import qualified Data.Text       as T

import qualified Pllisp.CST      as CST
import qualified Pllisp.Parser   as Parser
import qualified Pllisp.Resolve  as Resolve
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC
import qualified Pllisp.ClosureConvert as CC

spec :: Spec
spec = do
  describe "literals and simple values" $ do
    it "converts an integer literal" $ do
      let [Ty.Typed t (CC.CCLit (CST.LitInt 42))] = cc "42"
      t `shouldBe` Ty.TyInt

    it "converts a float literal" $ do
      let [Ty.Typed t (CC.CCLit (CST.LitFlt 3.14))] = cc "3.14"
      t `shouldBe` Ty.TyFlt

    it "converts a string literal" $ do
      let [Ty.Typed t (CC.CCLit (CST.LitStr "hi"))] = cc "\"hi\""
      t `shouldBe` Ty.TyStr

    it "converts a boolean" $ do
      let [Ty.Typed t (CC.CCBool True)] = cc "true"
      t `shouldBe` Ty.TyBool

    it "converts unit" $ do
      let [Ty.Typed t CC.CCUnit] = cc "unit"
      t `shouldBe` Ty.TyUnit

  describe "variables" $ do
    it "converts a variable reference" $ do
      let [Ty.Typed _ (CC.CCLet [(_, _, _)] (Ty.Typed t (CC.CCVar name _)))] =
            cc "(let ((x 1)) x)"
      t `shouldBe` Ty.TyInt
      name `shouldBe` "X"

  describe "if expressions" $ do
    it "converts if expression preserving type" $ do
      let [Ty.Typed t (CC.CCIf _ _ _)] = cc "(if true 1 2)"
      t `shouldBe` Ty.TyInt

    it "recursively converts if subexpressions" $ do
      let [Ty.Typed _ (CC.CCIf (Ty.Typed ct _) (Ty.Typed tt _) (Ty.Typed et _))] =
            cc "(if true 1 2)"
      ct `shouldBe` Ty.TyBool
      tt `shouldBe` Ty.TyInt
      et `shouldBe` Ty.TyInt

  describe "let expressions" $ do
    it "converts let bindings" $ do
      let [Ty.Typed t (CC.CCLet binds body)] = cc "(let ((x 1)) x)"
      t `shouldBe` Ty.TyInt
      length binds `shouldBe` 1

    it "converts multiple let bindings" $ do
      let [Ty.Typed _ (CC.CCLet binds _)] = cc "(let ((x 1) (y 2)) (add x y))"
      length binds `shouldBe` 2

  describe "application" $ do
    it "converts function application" $ do
      let [Ty.Typed t (CC.CCApp _ args)] = cc "(add 1 2)"
      t `shouldBe` Ty.TyInt
      length args `shouldBe` 2

  describe "lambda free variables" $ do
    it "closed lambda has no free vars" $ do
      let [Ty.Typed _ (CC.CCLam params fvs _ _)] = cc "(lam ((x %INT)) x)"
      map fst params `shouldBe` ["X"]
      fvs `shouldBe` []

    it "captures free variable from enclosing let" $ do
      let [Ty.Typed _ (CC.CCLet _ (Ty.Typed _ (CC.CCLam _ fvs _ _)))] =
            cc "(let ((y 1)) (lam (x) (add x y)))"
      map fst fvs `shouldBe` ["Y"]

    it "does not capture built-ins" $ do
      let [Ty.Typed _ (CC.CCLam _ fvs _ _)] = cc "(lam ((x %INT)) (add x 1))"
      fvs `shouldBe` []

    it "captures multiple free variables" $ do
      let [Ty.Typed _ (CC.CCLet _ (Ty.Typed _ (CC.CCLam _ fvs _ _)))] =
            cc "(let ((x 1) (y 2)) (lam (z) (add x y)))"
      L.sort (map fst fvs) `shouldBe` ["X", "Y"]

    it "does not capture the lambda's own params" $ do
      let [Ty.Typed _ (CC.CCLam _ fvs _ _)] =
            cc "(lam ((x %INT) (y %INT)) (add x y))"
      fvs `shouldBe` []

    it "captures free vars in nested lambdas" $ do
      -- (let ((x 1)) (lam (y) (lam (z) x)))
      -- inner lambda: x is free, y is free
      -- outer lambda: x is free
      let [Ty.Typed _ (CC.CCLet _ (Ty.Typed _ (CC.CCLam _ outerFvs _ innerBody)))] =
            cc "(let ((x 1)) (lam (y) (lam (z) x)))"
      map fst outerFvs `shouldBe` ["X"]
      let Ty.Typed _ (CC.CCLam _ innerFvs _ _) = innerBody
      -- inner lambda captures x (from the let) — but after closure conversion
      -- it sees x through the outer lambda's env, so it's still free from its perspective
      length innerFvs `shouldSatisfy` (>= 1)

  describe "type declarations" $ do
    it "passes through type declarations" $ do
      let [Ty.Typed _ (CC.CCType name _ _)] = cc "(type Foo () (Bar))"
      name `shouldBe` "FOO"

  describe "case expressions" $ do
    it "converts case expression" $ do
      let result = cc "(type M (a) (N) (J a)) (let ((x (J 1))) (case x ((N) 0) ((J y) y)))"
          Ty.Typed t _ = last result
      t `shouldBe` Ty.TyInt

    it "does not capture constructors as free variables" $ do
      let result = cc "(type M () (A) (B)) (lam (x) (case x ((A) 1) ((B) 2)))"
          Ty.Typed _ (CC.CCLam _ fvs _ _) = last result
      fvs `shouldBe` []

-- Helpers

cc :: T.Text -> CC.CCProgram
cc src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right prog -> case Resolve.resolve (CST.progExprs prog) of
    Left _       -> error "resolve error in test"
    Right resolved -> case TC.typecheck resolved of
      Left _     -> error "typecheck error in test"
      Right typed -> CC.closureConvert typed
