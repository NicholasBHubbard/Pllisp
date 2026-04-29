{-# LANGUAGE OverloadedStrings #-}

module MacroInterpSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.MacroInterp as MI
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.TypeCheck as TC

parseSExpr :: T.Text -> SExpr.SExpr
parseSExpr src = case Parser.parseSExprs "<test>" src of
  Right [s] -> s
  Right ss -> error ("expected 1 sexpr, got " ++ show (length ss))
  Left err -> error ("parse error: " ++ MP.errorBundlePretty err)

compileExpr :: T.Text -> Either String TC.TRExpr
compileExpr src = do
  sexprs <- case Parser.parseSExprs "<test>" src of
    Left err -> Left (MP.errorBundlePretty err)
    Right s -> Right s
  expanded <- MacroExpand.expand sexprs
  sx <- case expanded of
    [s] -> Right s
    ss -> Left ("expected 1 expanded form, got " ++ show (length ss))
  expr <- firstLeft SExpr.ceMsg (SExpr.toCompileExpr sx)
  let ctx = MacroExpand.csCtCtx MacroExpand.defaultState
      envs = MacroExpand.csCtEnvs MacroExpand.defaultState
      scope = M.keysSet ctx
  resolved <- firstLeft renderResolveErrs (Resolve.resolveWith scope M.empty [expr])
  (typed, _) <- firstLeft renderTypeErrs (TC.typecheckWith envs ctx resolved)
  case reverse typed of
    trExpr : _ -> Right trExpr
    [] -> Left "expected at least one typed compile-time expression"
  where
    firstLeft f = either (Left . f) Right
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

evalSrc :: T.Text -> Either String MI.MVal
evalSrc src = do
  trExpr <- compileExpr src
  MI.runInterpM (MI.evalTyped (MacroExpand.csEnv MacroExpand.defaultState) trExpr)

expandSrc :: T.Text -> Either String [SExpr.SExpr]
expandSrc src = do
  sexprs <- case Parser.parseSExprs "<test>" src of
    Left err -> Left (MP.errorBundlePretty err)
    Right s -> Right s
  MacroExpand.expand sexprs

shouldEvalTo :: T.Text -> MI.MVal -> Expectation
shouldEvalTo src expected = case evalSrc src of
  Right val -> val `shouldBe` expected
  Left err -> expectationFailure ("eval failed: " ++ err)

shouldFail :: T.Text -> Expectation
shouldFail src = case evalSrc src of
  Left _ -> pure ()
  Right val -> expectationFailure ("expected error, got: " ++ show val)

spec :: Spec
spec = do
  describe "default environment" $ do
    it "loads higher-level compile-time helpers as typed closures" $ do
      let expectClosure name = case M.lookup name (MacroExpand.csEnv MacroExpand.defaultState) of
            Just MI.MTypedClosure{} -> pure ()
            Just other -> expectationFailure ("expected typed closure for " ++ T.unpack name ++ ", got " ++ show other)
            Nothing -> expectationFailure ("missing " ++ T.unpack name)
      mapM_ expectClosure ["APPEND", "REVERSE", "MAP", "FILTER", "FOLDL"]

    it "exposes public syntax helpers instead of internal __CT names" $ do
      let expectPresent name = case M.lookup name (MacroExpand.csEnv MacroExpand.defaultState) of
            Nothing -> expectationFailure ("missing " ++ T.unpack name)
            Just _ -> pure ()
      mapM_ expectPresent
        [ "SYNTAX-INT"
        , "SYNTAX-LIFT"
        , "SYNTAX-CONS"
        , "SYNTAX-APPEND"
        , "SYNTAX-INT-VALUE"
        ]
      mapM_
        (\name -> M.lookup name (MacroExpand.csEnv MacroExpand.defaultState) `shouldBe` Nothing)
        [ "__CT-INT"
        , "__CT-LIFT"
        , "__CT-CONS"
        , "__CT-APPEND"
        ]

  describe "typed compile-time evaluation" $ do
    it "evaluates literals" $
      "42" `shouldEvalTo` MI.MInt 42

    it "evaluates let and lambda through the typed path" $
      "(let ((double (lam ((x %INT)) (add x x)))) (double 21))"
        `shouldEvalTo` MI.MInt 42

    it "evaluates case over runtime ADTs at compile time" $
      "(case (Just 42) ((Just x) x) (_ 0))"
        `shouldEvalTo` MI.MInt 42

    it "builds syntax with quote" $
      "(quote (a 1 :foo))" `shouldEvalTo`
        MI.MSyntax (MI.SyList [MI.SyAtom "A", MI.SyInt 1, MI.SyUSym "FOO"])

    it "builds syntax with quasiquote, unquote, and splice" $
      "(let ((x 42) (xs (quote (1 2)))) `(a ,x ,@xs b))"
        `shouldEvalTo`
          MI.MSyntax (MI.SyList [MI.SyAtom "A", MI.SyInt 42, MI.SyInt 1, MI.SyInt 2, MI.SyAtom "B"])

    it "matches syntax-case quoted literal patterns" $
      "(syntax-case (quote foo) ((quote foo) 42) (_ 0))"
        `shouldEvalTo` MI.MInt 42

    it "matches syntax-case list patterns with &rest binders" $
      "(syntax-case (quote (foo 1 2)) ((head &rest tail) (add 1 (syntax-length tail))) (_ 0))"
        `shouldEvalTo` MI.MInt 3

    it "compares syntax structurally" $
      "(syntax-equal? (quote (foo 1)) (quote (foo 1)))"
        `shouldEvalTo` MI.MBool True

    it "uses syntax constructors and accessors directly" $
      "(syntax-symbol-name (syntax-symbol \"foo\"))"
        `shouldEvalTo` MI.MStr "foo"

    it "extracts integer syntax values directly" $
      "(syntax-int-value (syntax-int 42))"
        `shouldEvalTo` MI.MInt 42

    it "uses prelude syntax-list helpers on quoted syntax" $
      "(append (quote (1 2)) (quote (3 4)))"
        `shouldEvalTo` MI.MSyntax (MI.SyList [MI.SyInt 1, MI.SyInt 2, MI.SyInt 3, MI.SyInt 4])

    it "maps over syntax lists with typed lambdas" $
      "(map (lam ((x %SYNTAX)) (syntax-int (add (syntax-int-value x) 10))) (quote (1 2 3)))"
        `shouldEvalTo` MI.MSyntax (MI.SyList [MI.SyInt 11, MI.SyInt 12, MI.SyInt 13])

    it "filters syntax lists with typed lambdas" $
      "(filter (lam ((x %SYNTAX)) (lt 1 (syntax-int-value x))) (quote (1 2 3)))"
        `shouldEvalTo` MI.MSyntax (MI.SyList [MI.SyInt 2, MI.SyInt 3])

    it "folds syntax lists with typed lambdas" $
      "(foldl (lam ((acc %INT) (x %SYNTAX)) (add acc (syntax-int-value x))) 0 (quote (1 2 3)))"
        `shouldEvalTo` MI.MInt 6

    it "supports refs at compile time" $
      "(let ((r (ref 0))) (let ((_ (set! r 7))) (deref r)))"
        `shouldEvalTo` MI.MInt 7

    it "supports regex matching at compile time" $
      "(rx-match (rx-compile \"[0-9]+\") \"abc123def\")"
        `shouldEvalTo` MI.MBool True

    it "supports regex find at compile time" $
      "(rx-find (rx-compile \"[0-9]+\") \"abc123def\")"
        `shouldEvalTo` MI.MStr "123"

    it "supports regex captures at compile time" $
      "(rx-captures (rx-compile \"([0-9]+)-([0-9]+)\") \"12-34\")"
        `shouldEvalTo` MI.MList [MI.MStr "12", MI.MStr "34"]

    it "supports regex substitution at compile time" $
      "(rx-sub (rx-compile \"[0-9]+\") \"X\" \"a1b22c333\")"
        `shouldEvalTo` MI.MStr "aXb22c333"

    it "supports global regex substitution at compile time" $
      "(rx-gsub (rx-compile \"[0-9]+\") \"X\" \"a1b22c333\")"
        `shouldEvalTo` MI.MStr "aXbXcX"

    it "supports regex splitting at compile time" $
      "(rx-split (rx-compile \",\") \"a,b,c\")"
        `shouldEvalTo` MI.MList [MI.MStr "a", MI.MStr "b", MI.MStr "c"]

    it "does not expose gensym anymore" $
      shouldFail "(gensym)"

    it "raises compile-time errors through error" $
      case evalSrc "(error \"boom\")" of
        Left err -> err `shouldContain` "boom"
        Right _ -> expectationFailure "expected error"

    it "rejects wrong syntax access at compile time" $
      shouldFail "(syntax-int-value (quote foo))"

    it "reports public syntax helper names in compile-time errors" $
      case evalSrc "(syntax-car (syntax-int 1))" of
        Left err -> err `shouldContain` "syntax-car"
        Right val -> expectationFailure ("expected error, got: " ++ show val)

    it "reports FFI as unavailable at compile time" $
      case evalSrc "(pll-print \"hello\")" of
        Left err -> err `shouldContain` "ffi not available at macro expansion time: PLL-PRINT"
        Right val -> expectationFailure ("expected error, got: " ++ show val)

  describe "conversion" $ do
    it "sexprToVal preserves syntax forms" $
      MI.sexprToVal (parseSExpr "(quote (a 1))")
        `shouldBe` MI.MSyntax (MI.SyList [MI.SyAtom "QUOTE", MI.SyList [MI.SyAtom "A", MI.SyInt 1]])

    it "valToSExpr round-trips atoms" $
      MI.valToSExpr (MI.MAtom "FOO")
        `shouldBe` Right (Loc.Located dummySpan (SExpr.SAtom "FOO"))

    it "valToSExpr round-trips quoted syntax lists" $
      MI.valToSExpr (MI.MSyntax (MI.SyList [MI.SyAtom "A", MI.SyInt 1]))
        `shouldBe`
          Right
            (Loc.Located dummySpan
              (SExpr.SList
                [ Loc.Located dummySpan (SExpr.SAtom "A")
                , Loc.Located dummySpan (SExpr.SInt 1)
                ]))

    it "valToSExpr round-trips quasiquote syntax forms" $
      MI.valToSExpr (MI.sexprToVal (parseSExpr "`(a ,x ,@xs)"))
        `shouldBe`
          Right
            (Loc.Located dummySpan
              (SExpr.SQuasi
                (Loc.Located dummySpan
                  (SExpr.SList
                    [ Loc.Located dummySpan (SExpr.SAtom "A")
                    , Loc.Located dummySpan
                        (SExpr.SUnquote (Loc.Located dummySpan (SExpr.SAtom "X")))
                    , Loc.Located dummySpan
                        (SExpr.SSplice (Loc.Located dummySpan (SExpr.SAtom "XS")))
                    ]))))

    it "valToSExpr rejects typed closures" $
      case compileExpr "1" of
        Left err -> expectationFailure err
        Right trExpr ->
          case MI.valToSExpr (MI.MTypedClosure M.empty [] trExpr) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error"

    it "valToSExpr rejects builtins" $
      case MI.valToSExpr (MI.MBuiltin "X" (\_ -> pure (MI.MInt 0))) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error"

    it "valToSExpr preserves type syntax" $ do
      let val = MI.MType (MI.MAtom "INT")
      MI.valToSExpr val
        `shouldBe`
          Right
            (Loc.Located dummySpan
              (SExpr.SType (Loc.Located dummySpan (SExpr.SAtom "INT"))))

  describe "macro expansion edge cases" $ do
    it "still rejects macros that return closures" $
      case expandSrc "(mac bad () (lam ((x %INT)) x)) (bad)" of
        Left msg -> msg `shouldContain` "closure"
        Right _ -> expectationFailure "expected error"

  where
    dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
