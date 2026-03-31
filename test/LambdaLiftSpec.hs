{-# LANGUAGE OverloadedStrings #-}

module LambdaLiftSpec (spec) where

import Test.Hspec

import qualified Data.List       as L
import qualified Data.Text       as T

import qualified Pllisp.CST           as CST
import qualified Pllisp.Parser        as Parser
import qualified Pllisp.Resolve       as Resolve
import qualified Pllisp.Type          as Ty
import qualified Pllisp.TypeCheck     as TC
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.LambdaLift    as LL

spec :: Spec
spec = do
  describe "literals and simple values" $ do
    it "integer literal passes through, no defs" $ do
      let result = ll "42"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed t (LL.LLLit (CST.LitInt 42))] = LL.llExprs result
      t `shouldBe` Ty.TyInt

    it "float literal passes through" $ do
      let result = ll "3.14"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed t (LL.LLLit (CST.LitFlt 3.14))] = LL.llExprs result
      t `shouldBe` Ty.TyFlt

    it "string literal passes through" $ do
      let result = ll "\"hi\""
      LL.llDefs result `shouldBe` []
      let [Ty.Typed t (LL.LLLit (CST.LitStr "hi"))] = LL.llExprs result
      t `shouldBe` Ty.TyStr

    it "boolean passes through" $ do
      let result = ll "true"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed t (LL.LLBool True)] = LL.llExprs result
      t `shouldBe` Ty.TyBool

    it "unit passes through" $ do
      let result = ll "unit"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed t LL.LLUnit] = LL.llExprs result
      t `shouldBe` Ty.TyUnit

  describe "variables" $ do
    it "variable in let passes through" $ do
      let result = ll "(let ((x 1)) x)"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed _ (LL.LLLet _ (Ty.Typed t (LL.LLVar name _)))] = LL.llExprs result
      t `shouldBe` Ty.TyInt
      name `shouldBe` "X"

  describe "closed lambda" $ do
    it "lifts to def with empty env" $ do
      let result = ll "(lam ((x %INT)) x)"
      length (LL.llDefs result) `shouldBe` 1
      let [def] = LL.llDefs result
      LL.defEnv def `shouldBe` []
      LL.defParams def `shouldBe` [("X", Ty.TyInt)]
      LL.defRetTy def `shouldBe` Ty.TyInt

    it "replaces with MkClosure with no env args" $ do
      let result = ll "(lam ((x %INT)) x)"
          [Ty.Typed _ (LL.LLMkClosure name args)] = LL.llExprs result
      args `shouldBe` []
      name `shouldBe` LL.defName (head (LL.llDefs result))

    it "def body contains the lifted body expression" $ do
      let result = ll "(lam ((x %INT)) x)"
          [def] = LL.llDefs result
          Ty.Typed bt (LL.LLVar bname _) = LL.defBody def
      bt `shouldBe` Ty.TyInt
      bname `shouldBe` "X"

    it "MkClosure preserves the function type" $ do
      let result = ll "(lam ((x %INT)) x)"
          [Ty.Typed closureTy (LL.LLMkClosure _ _)] = LL.llExprs result
      closureTy `shouldBe` Ty.TyFun [Ty.TyInt] Ty.TyInt

  describe "lambda with free vars" $ do
    it "lifts with env containing free vars" $ do
      let result = ll "(let ((y 1)) (lam (x) (add x y)))"
          [def] = LL.llDefs result
      map fst (LL.defEnv def) `shouldBe` ["Y"]
      map snd (LL.defEnv def) `shouldBe` [Ty.TyInt]

    it "MkClosure passes free var values" $ do
      let result = ll "(let ((y 1)) (lam (x) (add x y)))"
          [Ty.Typed _ (LL.LLLet _ (Ty.Typed _ (LL.LLMkClosure _ fvArgs)))] = LL.llExprs result
      length fvArgs `shouldBe` 1
      let [Ty.Typed ft (LL.LLVar fn _)] = fvArgs
      fn `shouldBe` "Y"
      ft `shouldBe` Ty.TyInt

  describe "nested lambdas" $ do
    it "both inner and outer are lifted" $ do
      let result = ll "(let ((x 1)) (lam (y) (lam (z) x)))"
      length (LL.llDefs result) `shouldBe` 2

    it "inner lambda is lifted first (appears first in defs)" $ do
      -- liftExpr recurses into body before emitting the outer def
      let result = ll "(let ((x 1)) (lam (y) (lam (z) x)))"
          [innerDef, outerDef] = LL.llDefs result
      -- inner takes z as param, outer takes y as param
      map fst (LL.defParams innerDef) `shouldBe` ["Z"]
      map fst (LL.defParams outerDef) `shouldBe` ["Y"]

  describe "lambda in let binding" $ do
    it "let-bound lambda is lifted" $ do
      let result = ll "(let ((f (lam ((x %INT)) x))) (f 1))"
      length (LL.llDefs result) `shouldBe` 1
      -- the let binding's RHS should be a MkClosure
      let [Ty.Typed _ (LL.LLLet [(_, _, rhs)] _)] = LL.llExprs result
          Ty.Typed _ (LL.LLMkClosure _ _) = rhs
      return ()

  describe "unique names" $ do
    it "multiple lambdas get distinct names" $ do
      let result = ll "(let ((f (lam ((x %INT)) x)) (g (lam ((y %INT)) y))) f)"
          names = map LL.defName (LL.llDefs result)
      length names `shouldBe` 2
      length (L.nub names) `shouldBe` 2

  describe "lambda in if branches" $ do
    it "lambdas in both branches are lifted" $ do
      let result = ll "(if true (lam ((x %INT)) x) (lam ((y %INT)) y))"
      length (LL.llDefs result) `shouldBe` 2
      -- both branches should be MkClosure
      let [Ty.Typed _ (LL.LLIf _ (Ty.Typed _ (LL.LLMkClosure _ _)) (Ty.Typed _ (LL.LLMkClosure _ _)))] = LL.llExprs result
      return ()

  describe "immediate application" $ do
    it "applied lambda is lifted" $ do
      let result = ll "((lam ((x %INT)) x) 42)"
      length (LL.llDefs result) `shouldBe` 1
      let [Ty.Typed _ (LL.LLApp (Ty.Typed _ (LL.LLMkClosure _ _)) [Ty.Typed _ (LL.LLLit (CST.LitInt 42))])] = LL.llExprs result
      return ()

  describe "lambda in case arm" $ do
    it "lambdas in case arms are lifted" $ do
      let result = ll "(case true (true (lam ((z %INT)) z)) (false (lam ((w %INT)) w)))"
      length (LL.llDefs result) `shouldBe` 2

  describe "type declarations" $ do
    it "pass through unchanged, no defs" $ do
      let result = ll "(type Foo () (Bar))"
      LL.llDefs result `shouldBe` []
      let [Ty.Typed _ (LL.LLType name _ _)] = LL.llExprs result
      name `shouldBe` "FOO"

  describe "case expressions" $ do
    it "case with no lambdas preserved structurally" $ do
      let result = ll "(type M (a) (N) (J a)) (let ((x (J 1))) (case x ((N) 0) ((J y) y)))"
      LL.llDefs result `shouldBe` []
      let exprs = LL.llExprs result
          Ty.Typed t _ = last exprs
      t `shouldBe` Ty.TyInt

  describe "free var types" $ do
    it "env entries have correct types" $ do
      let result = ll "(let ((y 1)) (lam (x) (add x y)))"
          [def] = LL.llDefs result
      LL.defEnv def `shouldBe` [("Y", Ty.TyInt)]

  describe "built-ins not in env" $ do
    it "closed lambda using builtins has empty env" $ do
      let result = ll "(lam ((x %INT)) (add x 1))"
          [def] = LL.llDefs result
      LL.defEnv def `shouldBe` []

  describe "deep nesting" $ do
    it "lambda in let in lambda produces 2 defs" $ do
      let result = ll "(let ((x 1)) (lam (y) (let ((z 2)) (lam (w) (add x z)))))"
      length (LL.llDefs result) `shouldBe` 2

    it "inner def captures both x and z" $ do
      let result = ll "(let ((x 1)) (lam (y) (let ((z 2)) (lam (w) (add x z)))))"
          innerDef = head (LL.llDefs result)
      L.sort (map fst (LL.defEnv innerDef)) `shouldBe` ["X", "Z"]

-- Helpers

ll :: T.Text -> LL.LLProgram
ll src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right prog -> case Resolve.resolve (CST.progExprs prog) of
    Left _       -> error "resolve error in test"
    Right resolved -> case TC.typecheck resolved of
      Left _     -> error "typecheck error in test"
      Right typed -> LL.lambdaLift (CC.closureConvert typed)
