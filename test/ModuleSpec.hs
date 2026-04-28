{-# LANGUAGE OverloadedStrings #-}

module ModuleSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
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

    it "top-level let splices bindings into nested chain" $ do
      let result = desugar "(let ((x 1)) x)"
      length result `shouldBe` 1
      -- (let ((x 1)) (let ((_ x)) unit))
      let Loc.Located _ (CST.ExprLet binds body) = head result
      length binds `shouldBe` 1
      CST.symName (fst (head binds)) `shouldBe` "X"
      -- body is let ((_ x)) unit
      case Loc.locVal body of
        CST.ExprLet innerBinds innerBody -> do
          length innerBinds `shouldBe` 1
          CST.symName (fst (head innerBinds)) `shouldBe` "_"
          Loc.locVal innerBody `shouldBe` CST.ExprUnit
        _ -> expectationFailure "expected nested let"

    it "multiple forms desugar into nested let chain" $ do
      -- (let ((x 1)) x) (add x 1) becomes:
      -- (let ((x 1)) (let ((_ x)) (let ((_ (add x 1))) unit)))
      let result = desugar "(let ((x 1)) x) (add x 1)"
      length result `shouldBe` 1
      let Loc.Located _ (CST.ExprLet binds1 body1) = head result
      length binds1 `shouldBe` 1
      CST.symName (fst (head binds1)) `shouldBe` "X"
      -- body1: (let ((_ x)) ...)
      let CST.ExprLet binds2 body2 = Loc.locVal body1
      length binds2 `shouldBe` 1
      CST.symName (fst (head binds2)) `shouldBe` "_"
      -- body2: (let ((_ (add x 1))) unit)
      let CST.ExprLet binds3 body3 = Loc.locVal body2
      length binds3 `shouldBe` 1
      CST.symName (fst (head binds3)) `shouldBe` "_"
      Loc.locVal body3 `shouldBe` CST.ExprUnit

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
    it "collects let binding schemes" $ do
      let exports = collectExports "(let ((x 1)) x)"
      M.lookup "X" exports `shouldBe` Just (TC.Forall S.empty Ty.TyInt)

    it "collects bindings across nested chain" $ do
      let exports = collectExports "(let ((x 1)) x) (let ((y 2)) y)"
      M.member "X" exports `shouldBe` True
      M.member "Y" exports `shouldBe` True

    it "excludes _ bindings from exports" $ do
      let exports = collectExports "(let ((_ 1)) unit)"
      M.member "_" exports `shouldBe` False

    it "collects type constructor exports" $ do
      let exports = collectExports "(type Foo () (Bar))"
      M.member "BAR" exports `shouldBe` True

    it "constructor exports have proper schemes" $ do
      let exports = collectExports "(type M (a) (N) (J a))"
      case M.lookup "J" exports of
        Just (TC.Forall _ (Ty.TyFun _ _)) -> pure ()
        other -> expectationFailure ("expected TyFun scheme for J, got: " ++ show other)
      case M.lookup "N" exports of
        Just (TC.Forall _ (Ty.TyCon "M" _)) -> pure ()
        other -> expectationFailure ("expected TyCon M for N, got: " ++ show other)

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

  describe "buildImportScope" $ do
    it "qualified-only import: ALIAS.NAME in scope, bare NAME not" $ do
      let exports = M.singleton "FOO" (M.singleton "BAR" (TC.Forall S.empty Ty.TyInt))
          imports = [CST.Import "FOO" "FOO" []]
          (resolveScope, tcCtx, normMap) = Mod.buildImportScope exports imports
      S.member "FOO.BAR" resolveScope `shouldBe` True
      S.member "BAR" resolveScope `shouldBe` False
      M.member "FOO.BAR" tcCtx `shouldBe` True
      -- BAR is in TC context (needed for normalization) but not in resolve scope
      M.member "BAR" tcCtx `shouldBe` True
      M.lookup "FOO.BAR" normMap `shouldBe` Just "BAR"

    it "import with unquals: both qualified and unqualified in scope" $ do
      let exports = M.singleton "FOO" (M.singleton "BAR" (TC.Forall S.empty Ty.TyInt))
          imports = [CST.Import "FOO" "FOO" ["BAR"]]
          (resolveScope, tcCtx, _normMap) = Mod.buildImportScope exports imports
      S.member "FOO.BAR" resolveScope `shouldBe` True
      S.member "BAR" resolveScope `shouldBe` True
      M.member "FOO.BAR" tcCtx `shouldBe` True
      M.member "BAR" tcCtx `shouldBe` True

    it "alias changes qualified prefix" $ do
      let exports = M.singleton "FOO" (M.singleton "BAR" (TC.Forall S.empty Ty.TyInt))
          imports = [CST.Import "FOO" "F" []]
          (resolveScope, tcCtx, normMap) = Mod.buildImportScope exports imports
      S.member "F.BAR" resolveScope `shouldBe` True
      S.member "FOO.BAR" resolveScope `shouldBe` False
      M.member "F.BAR" tcCtx `shouldBe` True
      M.member "FOO.BAR" tcCtx `shouldBe` False
      M.lookup "F.BAR" normMap `shouldBe` Just "BAR"

    it "alias with unquals" $ do
      let exports = M.singleton "FOO" (M.singleton "BAR" (TC.Forall S.empty Ty.TyInt))
          imports = [CST.Import "FOO" "F" ["BAR"]]
          (resolveScope, tcCtx, _normMap) = Mod.buildImportScope exports imports
      S.member "F.BAR" resolveScope `shouldBe` True
      S.member "BAR" resolveScope `shouldBe` True
      S.member "FOO.BAR" resolveScope `shouldBe` False

    it "original module name not usable when alias is set" $ do
      let exports = M.singleton "FOO" (M.singleton "BAR" (TC.Forall S.empty Ty.TyInt))
          imports = [CST.Import "FOO" "F" []]
          (resolveScope, _, _) = Mod.buildImportScope exports imports
      S.member "FOO.BAR" resolveScope `shouldBe` False
      S.member "F.BAR" resolveScope `shouldBe` True

    it "multiple imports with no collisions" $ do
      let exports = M.fromList
            [ ("A", M.singleton "X" (TC.Forall S.empty Ty.TyInt))
            , ("B", M.singleton "Y" (TC.Forall S.empty Ty.TyStr))
            ]
          imports = [CST.Import "A" "A" ["X"], CST.Import "B" "B" []]
          (resolveScope, tcCtx, _normMap) = Mod.buildImportScope exports imports
      S.member "A.X" resolveScope `shouldBe` True
      S.member "X" resolveScope `shouldBe` True
      S.member "B.Y" resolveScope `shouldBe` True
      S.member "Y" resolveScope `shouldBe` False
      -- TC context: A.X, X, B.Y, Y (all unquals in TC for normalization)
      M.size tcCtx `shouldBe` 4

  describe "checkImportCollisions" $ do
    it "accepts non-overlapping unquals" $ do
      let exports = M.fromList
            [ ("A", M.singleton "X" (TC.Forall S.empty Ty.TyInt))
            , ("B", M.singleton "Y" (TC.Forall S.empty Ty.TyStr))
            ]
          imports = [CST.Import "A" "A" ["X"], CST.Import "B" "B" ["Y"]]
      Mod.checkImportCollisions exports imports `shouldBe` Right ()

    it "detects unqualified name collision" $ do
      let exports = M.fromList
            [ ("A", M.singleton "X" (TC.Forall S.empty Ty.TyInt))
            , ("B", M.singleton "X" (TC.Forall S.empty Ty.TyStr))
            ]
          imports = [CST.Import "A" "A" ["X"], CST.Import "B" "B" ["X"]]
      case Mod.checkImportCollisions exports imports of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected collision error"

    it "no collision when same name is only qualified" $ do
      let exports = M.fromList
            [ ("A", M.singleton "X" (TC.Forall S.empty Ty.TyInt))
            , ("B", M.singleton "X" (TC.Forall S.empty Ty.TyStr))
            ]
          imports = [CST.Import "A" "A" ["X"], CST.Import "B" "B" []]
      Mod.checkImportCollisions exports imports `shouldBe` Right ()

  describe "mergeImportedCode" $ do
    it "prepends imported type decls" $ do
      let importedTyped = typecheckSrc "(type M (a) (N) (J a))"
          localTyped    = typecheckSrc "42"
          merged = Mod.mergeImportedCode [importedTyped] localTyped
      -- First element should be the type decl
      case head merged of
        Loc.Located _ (Ty.Typed _ (TC.TRType "M" _ _)) -> pure ()
        other -> expectationFailure ("expected TRType M, got: " ++ show other)

    it "merges imported let bindings wrapping local code" $ do
      let importedTyped = typecheckSrc "(let ((x 1)) x)"
          localTyped    = typecheckSrc "(let ((y 2)) y)"
          merged = Mod.mergeImportedCode [importedTyped] localTyped
      -- Imported X wraps the local code as an outer let
      case [e | e@(Loc.Located _ (Ty.Typed _ (TC.TRLet _ _))) <- merged] of
        [Loc.Located _ (Ty.Typed _ (TC.TRLet outerBinds body))] -> do
          let outerNames = [n | (n, _, _) <- outerBinds, n /= "_"]
          elem "X" outerNames `shouldBe` True
          -- Body should be the local code (a let with Y)
          case body of
            Loc.Located _ (Ty.Typed _ (TC.TRLet innerBinds _)) -> do
              let innerNames = [n | (n, _, _) <- innerBinds, n /= "_"]
              elem "Y" innerNames `shouldBe` True
            _ -> expectationFailure "expected local let as body"
        other -> expectationFailure ("expected 1 outer let, got " ++ show (length other))

    it "passes through local code when no imports have bindings" $ do
      let localTyped = typecheckSrc "(let ((x 1)) x)"
          merged = Mod.mergeImportedCode [] localTyped
          letExprs = [bs | Loc.Located _ (Ty.Typed _ (TC.TRLet bs _)) <- merged]
      case letExprs of
        [binds] -> do
          let names = [n | (n, _, _) <- binds, n /= "_"]
          elem "X" names `shouldBe` True
        _ -> expectationFailure "expected local let preserved"

  describe "module name validation" $ do
    it "accepts matching name and filename" $ do
      Mod.validateModuleName "FOO" "Foo.pll" `shouldBe` Nothing

    it "rejects mismatched name and filename" $ do
      Mod.validateModuleName "FOO" "Bar.pll" `shouldSatisfy` (/= Nothing)

    it "handles path in filename" $ do
      Mod.validateModuleName "FOO" "src/Foo.pll" `shouldBe` Nothing

-- Helpers

desugar :: T.Text -> CST.CST
desugar src = case Mod.desugarTopLevel (CST.progExprs (parse src)) of
  Left e  -> error ("desugar: " ++ e)
  Right r -> r

parse :: T.Text -> CST.Program
parse src = case Parser.parseProgram "<test>" src of
  Left err  -> error (show err)
  Right prog -> prog

typecheckSrc :: T.Text -> TC.TResolvedCST
typecheckSrc src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right prog -> case Resolve.resolve S.empty (CST.progExprs prog) of
    Left _       -> error "resolve error in test"
    Right resolved -> case TC.typecheck M.empty resolved of
      Left _     -> error "typecheck error in test"
      Right typed -> typed

collectExports :: T.Text -> M.Map CST.Symbol TC.Scheme
collectExports src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right prog -> case Resolve.resolve S.empty (CST.progExprs prog) of
    Left _       -> error "resolve error in test"
    Right resolved -> case TC.typecheck M.empty resolved of
      Left _     -> error "typecheck error in test"
      Right typed -> Mod.collectExports TC.emptyTCEnvs typed
