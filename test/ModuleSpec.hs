{-# LANGUAGE OverloadedStrings #-}

module ModuleSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Pllisp.CST      as CST
import qualified Pllisp.SrcLoc   as Loc
import qualified Pllisp.Parser   as Parser
import qualified Pllisp.Resolve  as Resolve
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC
import qualified Pllisp.Module   as Mod

spec :: Spec
spec = do
  describe "top-level desugaring" $ do
    it "single bare expression becomes let _ = expr in unit" $ do
      let result = desugar "42"
      length result `shouldBe` 1
      let Loc.Located _ (CST.ExprLet binds body) = head result
      length binds `shouldBe` 1
      CST.symName (fst (head binds)) `shouldBe` "_"
      Loc.locVal body `shouldBe` CST.ExprUnit

    it "top-level let flattens bindings and body becomes _ binding" $ do
      let result = desugar "(let ((x 1)) x)"
      length result `shouldBe` 1
      let Loc.Located _ (CST.ExprLet binds body) = head result
      length binds `shouldBe` 2
      CST.symName (fst (head binds)) `shouldBe` "X"
      CST.symName (fst (binds !! 1)) `shouldBe` "_"
      Loc.locVal body `shouldBe` CST.ExprUnit

    it "multiple forms desugar into flat let" $ do
      let result = desugar "(let ((x 1)) x) (add x 1) (let ((y 2)) y) (add y 1)"
      length result `shouldBe` 1
      let Loc.Located _ (CST.ExprLet binds body) = head result
      length binds `shouldBe` 6
      CST.symName (fst (head binds)) `shouldBe` "X"
      CST.symName (fst (binds !! 1)) `shouldBe` "_"  -- body of first let (x)
      CST.symName (fst (binds !! 2)) `shouldBe` "_"  -- bare (add x 1)
      CST.symName (fst (binds !! 3)) `shouldBe` "Y"
      CST.symName (fst (binds !! 4)) `shouldBe` "_"  -- body of second let (y)
      CST.symName (fst (binds !! 5)) `shouldBe` "_"  -- bare (add y 1)
      Loc.locVal body `shouldBe` CST.ExprUnit

    it "type declarations stay separate from the let" $ do
      let result = desugar "(type Foo () (Bar)) 42"
      length result `shouldBe` 2
      let Loc.Located _ typExpr = head result
      case typExpr of
        CST.ExprType "FOO" _ _ -> pure ()
        _ -> expectationFailure ("expected type decl, got: " ++ show typExpr)
      let Loc.Located _ (CST.ExprLet binds body) = result !! 1
      length binds `shouldBe` 1
      Loc.locVal body `shouldBe` CST.ExprUnit

    it "interleaved types and exprs preserve type order" $ do
      let result = desugar "(type A () (X)) (let ((x 1)) x) (type B () (Y)) (let ((y 2)) y)"
      -- types stay in order, all non-type forms fold into one let at end
      length result `shouldBe` 3
      let Loc.Located _ t1 = result !! 0
          Loc.Located _ t2 = result !! 1
          Loc.Located _ l  = result !! 2
      case t1 of { CST.ExprType "A" _ _ -> pure (); _ -> expectationFailure "expected type A" }
      case t2 of { CST.ExprType "B" _ _ -> pure (); _ -> expectationFailure "expected type B" }
      case l of { CST.ExprLet _ _ -> pure (); _ -> expectationFailure "expected let" }

    it "empty program desugars to empty list" $ do
      let result = desugar ""
      result `shouldBe` []

    it "only type declarations, no let generated" $ do
      let result = desugar "(type Foo () (Bar))"
      length result `shouldBe` 1
      case Loc.locVal (head result) of
        CST.ExprType "FOO" _ _ -> pure ()
        other -> expectationFailure ("expected type, got: " ++ show other)

  describe "program structure" $ do
    it "creates anonymous program from plain expressions" $ do
      let prog = parse "42"
      CST.progName prog `shouldBe` Nothing
      CST.progImports prog `shouldBe` []
      length (CST.progExprs prog) `shouldBe` 1

  describe "export collection" $ do
    it "collects let binding types" $ do
      let exports = collectExports "(let ((x 1)) x)"
      M.lookup "X" exports `shouldBe` Just Ty.TyInt

    it "excludes _ bindings from exports" $ do
      let exports = collectExports "(let ((_ 1)) unit)"
      M.member "_" exports `shouldBe` False

    it "collects type constructor exports" $ do
      let exports = collectExports "(type Foo () (Bar))"
      M.member "BAR" exports `shouldBe` True

  describe "dependency ordering" $ do
    it "orders independent modules" $ do
      let deps = M.fromList [("A", []), ("B", [])]
      case Mod.dependencyOrder deps of
        Left err -> expectationFailure err
        Right order -> length order `shouldBe` 2

    it "orders dependent modules" $ do
      let deps = M.fromList [("A", ["B"]), ("B", [])]
      case Mod.dependencyOrder deps of
        Left err -> expectationFailure err
        Right order -> order `shouldBe` ["B", "A"]

    it "detects circular imports" $ do
      let deps = M.fromList [("A", ["B"]), ("B", ["A"])]
      case Mod.dependencyOrder deps of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected cycle error"

    it "detects indirect circular imports" $ do
      let deps = M.fromList [("A", ["B"]), ("B", ["C"]), ("C", ["A"])]
      case Mod.dependencyOrder deps of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected cycle error"

  describe "module name validation" $ do
    it "accepts matching name and filename" $ do
      Mod.validateModuleName "FOO" "Foo.pll" `shouldBe` Nothing

    it "rejects mismatched name and filename" $ do
      Mod.validateModuleName "FOO" "Bar.pll" `shouldSatisfy` (/= Nothing)

    it "handles path in filename" $ do
      Mod.validateModuleName "FOO" "src/Foo.pll" `shouldBe` Nothing

-- Helpers

desugar :: T.Text -> CST.CST
desugar src = Mod.desugarTopLevel (CST.progExprs (parse src))

parse :: T.Text -> CST.Program
parse src = case Parser.parseProgram "<test>" src of
  Left err  -> error (show err)
  Right prog -> prog

collectExports :: T.Text -> M.Map CST.Symbol Ty.Type
collectExports src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right prog -> case Resolve.resolve (CST.progExprs prog) of
    Left _       -> error "resolve error in test"
    Right resolved -> case TC.typecheck resolved of
      Left _     -> error "typecheck error in test"
      Right typed -> Mod.collectExports typed
