{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec (spec) where

import Test.Hspec

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import System.IO.Unsafe (unsafePerformIO)

import qualified Pllisp.CST      as CST
import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.Module   as Mod
import qualified Pllisp.Parser   as Parser
import qualified Pllisp.Resolve  as Resolve
import qualified Pllisp.SExpr    as SExpr
import qualified Pllisp.SrcLoc   as Loc
import qualified Pllisp.Stdlib   as Stdlib
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC

spec :: Spec
spec = do
  describe "error collection" $ do
    it "collects multiple type mismatch errors" $ do
      case parseAndTypecheck "(add true false)" of
        Right _ -> expectationFailure "expected type errors"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "collects errors from unifyMany" $ do
      case parseAndTypecheck "(add true \"x\")" of
        Right _ -> expectationFailure "expected type errors"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "hits the inferErrs-only branch via undefined variable" $ do
      -- Manually construct a ResolvedCST with an unresolved variable
      -- so infer records an error but solveAll succeeds → Left inferErrs path
      let sp    = Loc.Span (Loc.Pos "<test>" 1 1) (Loc.Pos "<test>" 1 1)
          ghost = Loc.Located sp (Resolve.RVar (Resolve.VarBinding (-1) "GHOST"))
      case TC.typecheck M.empty [ghost] of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "type mismatch in if condition" $ do
      case parseAndTypecheck "(if 42 1 2)" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "if branch type mismatch" $ do
      case parseAndTypecheck "(if true 1 \"x\")" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "wrong arity in application" $ do
      case parseAndTypecheck "(add 1 2 3)" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "constructor arity mismatch in pattern" $ do
      case parseAndTypecheck "(type P () (Pair %INT %INT)) (let ((x (Pair 1 2))) (case x ((Pair z) z)))" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "reports a field-owner mismatch clearly" $ do
      let src = T.unlines
            [ "(type Person ()"
            , "  (Person (name %STR) (age %INT)))"
            , ""
            , "(type Pet ()"
            , "  (Pet (kind %STR)))"
            , ""
            , "(let ((animal (Pet \"cat\")))"
            , "  (.age animal))"
            ]
      case parseAndTypecheckViaSExpr src of
        Right _ -> expectationFailure "expected type error"
        Left errs ->
          map TC.teMsg errs `shouldSatisfy`
            any (isInfixOf "field 'AGE' belongs to %PERSON, but expression has type %PET")

  describe "valid programs" $ do
    it "typechecks arithmetic expressions" $ do
      case parseAndTypecheck "(add 1 2)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "typechecks float arithmetic" $ do
      case parseAndTypecheck "(addf 1.0 2.0)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyFlt

    it "typechecks float comparison (EQF)" $ do
      case parseAndTypecheck "(eqf 1.0 2.0)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyBool

    it "typechecks float comparison (LTF)" $ do
      case parseAndTypecheck "(ltf 1.0 2.0)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyBool

    it "rejects int where float expected in EQF" $ do
      case parseAndTypecheck "(eqf 1 2.0)" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "typechecks NEGF as FLT -> FLT" $ do
      case parseAndTypecheck "(negf 1.0)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyFlt

    it "typechecks EQS as STR -> STR -> BOOL" $ do
      case parseAndTypecheck "(eqs \"hello\" \"world\")" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyBool

    it "typechecks READ-LINE as UNIT -> STR" $ do
      case parseAndTypecheck "(read-line unit)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyStr

    it "rejects int where string expected in EQS" $ do
      case parseAndTypecheck "(eqs 1 \"x\")" of
        Right _ -> expectationFailure "expected type error"
        Left errs -> length errs `shouldSatisfy` (>= 1)

    it "typechecks int literal" $ do
      case parseAndTypecheck "42" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "typechecks float literal" $ do
      case parseAndTypecheck "3.14" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyFlt

    it "typechecks string literal" $ do
      case parseAndTypecheck "\"hello\"" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyStr

    it "typechecks bool" $ do
      case parseAndTypecheck "true" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyBool

    it "typechecks unit" $ do
      case parseAndTypecheck "unit" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyUnit

    it "typechecks rx literal" $ do
      case parseAndTypecheck "/foo/" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyRx

    it "typechecks rx literal with flags" $ do
      case parseAndTypecheck "/\\d+/i" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyRx

    it "typechecks PRINT as STR -> UNIT" $ do
      case parseAndTypecheck "(print \"hello\")" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyUnit

    it "typechecks let bindings" $ do
      case parseAndTypecheck "(let ((x 42)) x)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "let binding of applied function infers correct type (not over-generalized)" $ do
      case parseAndTypecheck "(let ((x (add 1 2))) x)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "let binding of IO result infers unit" $ do
      case parseAndTypecheck "(let ((x (print \"hello\"))) x)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyUnit

    it "typechecks if expressions" $ do
      case parseAndTypecheck "(if true 1 2)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "typechecks lambda (inferred)" $ do
      case parseAndTypecheck "(lam (x) x)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> case topType typed of
          Ty.TyFun [_] _ -> pure ()
          t -> expectationFailure ("expected TyFun, got: " ++ show t)

    it "typechecks lambda with annotation" $ do
      case parseAndTypecheck "(lam ((x %INT)) x)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyFun [Ty.TyInt] Ty.TyInt

    it "typechecks polymorphic let" $ do
      case parseAndTypecheck "(let ((id (lam (x) x))) (add (id 1) (id 2)))" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "typechecks TRType node" $ do
      case parseAndTypecheck "(type Foo () (Bar))" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyCon "FOO" []

    it "typechecks case expression" $ do
      let src = "(type M (a) (N) (J a)) (let ((x (J 1))) (case x ((N) 0) ((J y) y)))"
      case parseAndTypecheck src of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> last (map topType' typed) `shouldBe` Ty.TyInt

  describe "imported context" $ do
    it "typechecks expression using imported scheme" $ do
      let importedCtx = M.singleton "FOO.BAR"
            (TC.Forall S.empty (Ty.TyFun [Ty.TyInt] Ty.TyInt))
      case parseAndTypecheckWith (S.singleton "FOO.BAR") importedCtx "(Foo.bar 42)" of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "rejects mistyped use of imported scheme" $ do
      let importedCtx = M.singleton "FOO.BAR"
            (TC.Forall S.empty (Ty.TyFun [Ty.TyInt] Ty.TyInt))
      case parseAndTypecheckWith (S.singleton "FOO.BAR") importedCtx "(Foo.bar \"hello\")" of
        Right _ -> expectationFailure "expected type error"
        Left _  -> pure ()

  describe "unification" $ do
    it "unify TyVar left with concrete type" $ do
      let sp = dummySpan
      TC.unify sp (Ty.TyVar 0) Ty.TyInt `shouldBe` Right (M.singleton 0 Ty.TyInt)

    it "unify TyVar right with concrete type" $ do
      let sp = dummySpan
      TC.unify sp Ty.TyInt (Ty.TyVar 0) `shouldBe` Right (M.singleton 0 Ty.TyInt)

    it "unify TyInt with TyInt" $ do
      TC.unify dummySpan Ty.TyInt Ty.TyInt `shouldBe` Right M.empty

    it "unify TyFlt with TyFlt" $ do
      TC.unify dummySpan Ty.TyFlt Ty.TyFlt `shouldBe` Right M.empty

    it "unify TyStr with TyStr" $ do
      TC.unify dummySpan Ty.TyStr Ty.TyStr `shouldBe` Right M.empty

    it "unify TyBool with TyBool" $ do
      TC.unify dummySpan Ty.TyBool Ty.TyBool `shouldBe` Right M.empty

    it "unify TyUnit with TyUnit" $ do
      TC.unify dummySpan Ty.TyUnit Ty.TyUnit `shouldBe` Right M.empty

    it "unify TyRx with TyRx" $ do
      TC.unify dummySpan Ty.TyRx Ty.TyRx `shouldBe` Right M.empty

    it "unify TyFun recursive" $ do
      let sp = dummySpan
      case TC.unify sp (Ty.TyFun [Ty.TyVar 0] (Ty.TyVar 1))
                       (Ty.TyFun [Ty.TyInt]    Ty.TyBool) of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right s -> do
          TC.apply s (Ty.TyVar 0) `shouldBe` Ty.TyInt
          TC.apply s (Ty.TyVar 1) `shouldBe` Ty.TyBool

    it "unify TyCon same name" $ do
      TC.unify dummySpan (Ty.TyCon "FOO" []) (Ty.TyCon "FOO" [])
        `shouldBe` Right M.empty

    it "unify TyCon different names → error" $ do
      case TC.unify dummySpan (Ty.TyCon "FOO" []) (Ty.TyCon "BAR" []) of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _   -> expectationFailure "expected unify error"

    it "unify Int with Bool → error" $ do
      case TC.unify dummySpan Ty.TyInt Ty.TyBool of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _   -> expectationFailure "expected unify error"

    it "unify TyApp with matching structure" $ do
      case TC.unify dummySpan
             (Ty.TyApp (Ty.TyVar 0) Ty.TyInt)
             (Ty.TyApp (Ty.TyCon "MAYBE" []) Ty.TyInt) of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right s -> TC.apply s (Ty.TyVar 0) `shouldBe` Ty.TyCon "MAYBE" []

    it "unify TyApp mismatch" $ do
      case TC.unify dummySpan
             (Ty.TyApp (Ty.TyVar 0) Ty.TyInt)
             Ty.TyBool of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected unify error"

    it "unify TyApp vs TyCon decomposes" $ do
      -- TyApp (TyVar 0) TyInt ~ TyCon "MAYBE" [TyInt]
      -- should decompose TyCon to TyApp and solve TyVar 0 = TyCon "MAYBE" []
      case TC.unify dummySpan
             (Ty.TyApp (Ty.TyVar 0) (Ty.TyVar 1))
             (Ty.TyCon "MAYBE" [Ty.TyInt]) of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right s -> do
          TC.apply s (Ty.TyVar 0) `shouldBe` Ty.TyCon "MAYBE" []
          TC.apply s (Ty.TyVar 1) `shouldBe` Ty.TyInt

  describe "TyApp substitution" $ do
    it "apply substitution to TyApp" $ do
      let subst = M.singleton 0 (Ty.TyCon "MAYBE" [])
      TC.apply subst (Ty.TyApp (Ty.TyVar 0) Ty.TyInt)
        `shouldBe` Ty.TyApp (Ty.TyCon "MAYBE" []) Ty.TyInt

    it "tvs collects variables from TyApp" $ do
      TC.tvs (Ty.TyApp (Ty.TyVar 0) (Ty.TyVar 1))
        `shouldBe` S.fromList [0, 1]

  describe "bind" $ do
    it "bind TyVar to concrete type" $ do
      TC.bind dummySpan 0 Ty.TyInt `shouldBe` Right (M.singleton 0 Ty.TyInt)

    it "bind detects infinite type" $ do
      case TC.bind dummySpan 0 (Ty.TyFun [Ty.TyVar 0] Ty.TyInt) of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _   -> expectationFailure "expected infinite type error"

  describe "unifyMany" $ do
    it "empty lists" $ do
      TC.unifyMany dummySpan [] [] `shouldBe` Right M.empty

    it "arity mismatch" $ do
      case TC.unifyMany dummySpan [Ty.TyInt] [] of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _   -> expectationFailure "expected arity error"

    it "collects errors from multiple pairs" $ do
      case TC.unifyMany dummySpan [Ty.TyInt, Ty.TyStr] [Ty.TyBool, Ty.TyFlt] of
        Left errs -> length errs `shouldBe` 2
        Right _   -> expectationFailure "expected errors"

  describe "solveAll" $ do
    it "empty constraints" $ do
      TC.solveAll [] `shouldBe` Right M.empty

    it "single satisfiable constraint" $ do
      let c = TC.Constraint dummySpan (Ty.TyVar 0) Ty.TyInt
      case TC.solveAll [c] of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right s   -> TC.apply s (Ty.TyVar 0) `shouldBe` Ty.TyInt

    it "collects all errors from unsatisfiable constraints" $ do
      let c1 = TC.Constraint dummySpan Ty.TyInt  Ty.TyBool
          c2 = TC.Constraint dummySpan Ty.TyStr  Ty.TyFlt
      case TC.solveAll [c1, c2] of
        Left errs -> length errs `shouldBe` 2
        Right _   -> expectationFailure "expected errors"

  describe "solve" $ do
    it "empty constraints" $ do
      TC.solve [] `shouldBe` Right M.empty

    it "single satisfiable constraint" $ do
      let c = TC.Constraint dummySpan (Ty.TyVar 0) Ty.TyInt
      case TC.solve [c] of
        Left errs -> expectationFailure (show (map TC.teMsg errs))
        Right s   -> TC.apply s (Ty.TyVar 0) `shouldBe` Ty.TyInt

    it "short-circuits on first error" $ do
      let c1 = TC.Constraint dummySpan Ty.TyInt Ty.TyBool
          c2 = TC.Constraint dummySpan Ty.TyStr Ty.TyFlt
      case TC.solve [c1, c2] of
        Left errs -> length errs `shouldBe` 1
        Right _   -> expectationFailure "expected error"

  describe "higher-kinded types" $ do
    it "HKT class resolves type params correctly" $ do
      let src = T.unlines
            [ "(cls FUNCTOR () (f)"
            , "  (fmap %(-> a b) %(f a) %(f b)))"
            , "(type Box (a) (MkBox a))"
            , "(inst FUNCTOR %Box"
            , "  (fmap (lam ((fn %(-> a b)) (box %(Box a)))"
            , "    (case box ((MkBox x) (MkBox (fn x)))))))"
            , "(case (fmap (lam ((x %INT)) (add x 1)) (MkBox 41))"
            , "  ((MkBox y) y))"
            ]
      case parseAndTypecheck src of
        Left errs -> expectationFailure (unlines (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyInt

    it "Monad bind infers correct type" $ do
      let src = T.unlines
            [ "(type Opt (a) (Some a) (None))"
            , "(cls MONAD () (m)"
            , "  (bind %(m a) %(-> a (m b)) %(m b)))"
            , "(inst MONAD %Opt"
            , "  (bind (lam ((mx %(Opt a)) (fn %(-> a (Opt b))))"
            , "    (case mx"
            , "      ((None) None)"
            , "      ((Some x) (fn x))))))"
            , "(bind (Some 42) (lam ((x %INT)) (Some (add x 1))))"
            ]
      case parseAndTypecheck src of
        Left errs -> expectationFailure (unlines (map TC.teMsg errs))
        Right typed -> topType typed `shouldBe` Ty.TyApp (Ty.TyCon "OPT" []) Ty.TyInt

  describe "kind validation" $ do
    it "rejects ground type as HKT class instance" $ do
      let src = T.unlines
            [ "(cls FUNCTOR () (f)"
            , "  (fmap %(-> a b) %(f a) %(f b)))"
            , "(inst FUNCTOR %INT"
            , "  (fmap (lam ((fn %(-> a b)) (x %INT)) x)))"
            ]
      case parseAndTypecheck src of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected kind error for INT as FUNCTOR instance"

    it "accepts type constructor as HKT class instance" $ do
      let src = T.unlines
            [ "(cls FUNCTOR () (f)"
            , "  (fmap %(-> a b) %(f a) %(f b)))"
            , "(type Box (a) (MkBox a))"
            , "(inst FUNCTOR %Box"
            , "  (fmap (lam ((fn %(-> a b)) (box %(Box a)))"
            , "    (case box ((MkBox x) (MkBox (fn x)))))))"
            ]
      case parseAndTypecheck src of
        Left errs -> expectationFailure (unlines (map TC.teMsg errs))
        Right _   -> pure ()

    it "rejects ground type for multi-param HKT" $ do
      let src = T.unlines
            [ "(cls MAPPABLE () (f)"
            , "  (mmap %(-> a b) %(f a) %(f b)))"
            , "(inst MAPPABLE %BOOL"
            , "  (mmap (lam ((fn %(-> a b)) (x %BOOL)) x)))"
            ]
      case parseAndTypecheck src of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected kind error for BOOL as MAPPABLE instance"

  describe "superclass constraints" $ do
    it "rejects instance when superclass instance is missing" $ do
      let src = T.unlines
            [ "(cls FUNCTOR () (f) (fmap %(-> a b) %(f a) %(f b)))"
            , "(cls APPLICATIVE (FUNCTOR) (f)"
            , "  (pure %a %(f a))"
            , "  (ap %(f %(-> a b)) %(f a) %(f b)))"
            , "(type Box (a) (MkBox a))"
            , "(inst APPLICATIVE %Box"
            , "  (pure (lam ((x %a)) (MkBox x)))"
            , "  (ap (lam ((mf %(Box %(-> a b))) (mx %(Box a)))"
            , "    (case mf ((MkBox fn) (case mx ((MkBox x) (MkBox (fn x)))))))))"
            ]
      case parseAndTypecheck src of
        Right _ -> expectationFailure "expected superclass error"
        Left errs -> any (\e -> "FUNCTOR" `isInfixOf` TC.teMsg e) errs `shouldBe` True

    it "accepts instance when superclass instance exists" $ do
      let src = T.unlines
            [ "(cls FUNCTOR () (f) (fmap %(-> a b) %(f a) %(f b)))"
            , "(cls APPLICATIVE (FUNCTOR) (f)"
            , "  (pure %a %(f a))"
            , "  (ap %(f %(-> a b)) %(f a) %(f b)))"
            , "(type Box (a) (MkBox a))"
            , "(inst FUNCTOR %Box"
            , "  (fmap (lam ((fn %(-> a b)) (box %(Box a)))"
            , "    (case box ((MkBox x) (MkBox (fn x)))))))"
            , "(inst APPLICATIVE %Box"
            , "  (pure (lam ((x %a)) (MkBox x)))"
            , "  (ap (lam ((mf %(Box %(-> a b))) (mx %(Box a)))"
            , "    (case mf ((MkBox fn) (case mx ((MkBox x) (MkBox (fn x)))))))))"
            ]
      case parseAndTypecheck src of
        Left errs -> expectationFailure (unlines (map TC.teMsg errs))
        Right _ -> pure ()

    it "class with empty superclass list works" $ do
      let src = T.unlines
            [ "(cls SHOW () (a) (show %a %STR))"
            , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
            , "(print (show 42))"
            ]
      case parseAndTypecheck src of
        Left errs -> expectationFailure (unlines (map TC.teMsg errs))
        Right _ -> pure ()

    it "rejects cls missing superclass list" $ do
      -- Old syntax without () should now fail to parse
      case Parser.parseProgram "<test>" "(cls SHOW (a) (show %a %STR))" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected parse error for missing superclass list"

  describe "mutable refs" $ do
    it "ref infers Ref a" $
      case parseAndTypecheck "(ref 42)" of
        Right r -> topType r `shouldBe` Ty.TyCon "REF" [Ty.TyInt]
        Left e  -> expectationFailure (show e)

    it "deref unwraps Ref a to a" $
      case parseAndTypecheck "(deref (ref 42))" of
        Right r -> topType r `shouldBe` Ty.TyInt
        Left e  -> expectationFailure (show e)

    it "set! returns Unit" $
      case parseAndTypecheck "(let ((r (ref 0))) (set! r 1))" of
        Right r -> topType r `shouldBe` Ty.TyUnit
        Left e  -> expectationFailure (show e)

    it "rejects deref on non-ref" $
      case parseAndTypecheck "(deref 42)" of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected type error"

    it "rejects set! with wrong value type" $
      case parseAndTypecheck "(let ((r (ref 0))) (set! r \"hello\"))" of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected type error"

-- Helpers

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "<test>" 1 1) (Loc.Pos "<test>" 1 1)

data PreludeSupport = PreludeSupport
  { psScope :: S.Set CST.Symbol
  , psCtx   :: M.Map CST.Symbol TC.Scheme
  , psEnvs  :: TC.TCEnvs
  }

preludeSupport :: PreludeSupport
preludeSupport = unsafePerformIO $ do
  preludeSexprs <- Stdlib.loadPrelude
  let expanded = case MacroExpand.expandWith MacroExpand.primitiveState preludeSexprs of
        Left err -> error ("prelude macro error in test: " ++ err)
        Right sexprs -> sexprs
      prog = case SExpr.toProgram expanded of
        Left err -> error ("prelude sexpr error in test: " ++ SExpr.ceMsg err)
        Right parsed -> parsed
  case Mod.desugarTopLevel (CST.progExprs prog) of
    Left err -> error ("prelude desugar error in test: " ++ err)
    Right exprs -> case Resolve.resolve S.empty exprs of
      Left _ -> error "prelude resolve error in test"
      Right resolved -> case TC.typecheckWith TC.emptyTCEnvs M.empty resolved of
        Left errs -> error ("prelude typecheck error in test: " ++ show errs)
        Right (typed, envs) ->
          let exports = Mod.collectExports envs typed
          in pure PreludeSupport
               { psScope = M.keysSet exports
               , psCtx = exports
               , psEnvs = envs
               }
{-# NOINLINE preludeSupport #-}

parseAndTypecheck :: T.Text -> Either [TC.TypeError] TC.TResolvedCST
parseAndTypecheck = parseAndTypecheckWith S.empty M.empty

parseAndTypecheckWith :: S.Set CST.Symbol -> M.Map CST.Symbol TC.Scheme -> T.Text -> Either [TC.TypeError] TC.TResolvedCST
parseAndTypecheckWith importedNames importedCtx src = case Parser.parseProgram "<test>" src of
  Left _     -> error "parse error in test"
  Right prog -> case Resolve.resolve (S.union (psScope preludeSupport) importedNames) (CST.progExprs prog) of
      Left _       -> error "resolve error in test"
      Right resolved ->
        fmap fst $
          TC.typecheckWith (psEnvs preludeSupport) (M.union importedCtx (psCtx preludeSupport)) resolved

parseAndTypecheckViaSExpr :: T.Text -> Either [TC.TypeError] TC.TResolvedCST
parseAndTypecheckViaSExpr src = case Parser.parseSExprs "<test>" src of
  Left _ -> error "parse error in test"
  Right sexprs -> case SExpr.toProgram sexprs of
    Left err -> error ("sexpr error in test: " ++ SExpr.ceMsg err)
    Right prog -> case Resolve.resolve (psScope preludeSupport) (CST.progExprs prog) of
        Left _ -> error "resolve error in test"
        Right resolved ->
          fmap fst $
            TC.typecheckWith (psEnvs preludeSupport) (psCtx preludeSupport) resolved

topType :: TC.TResolvedCST -> Ty.Type
topType [] = error "empty TResolvedCST"
topType xs = topType' (last xs)

topType' :: TC.TRExpr -> Ty.Type
topType' (Loc.Located _ (Ty.Typed t _)) = t
