{-# LANGUAGE OverloadedStrings #-}

module ExhaustCheckSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Pllisp.CST      as CST
import qualified Pllisp.ExhaustCheck as Exhaust
import qualified Pllisp.Parser   as Parser
import qualified Pllisp.Resolve  as Resolve
import qualified Pllisp.SrcLoc   as Loc
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC

spec :: Spec
spec = do
  describe "renderPat" $ do
    it "0-arg ctor" $
      Exhaust.renderPat "NOTHING" 0 `shouldBe` "(NOTHING)"

    it "1-arg ctor" $
      Exhaust.renderPat "JUST" 1 `shouldBe` "(JUST _)"

    it "2-arg ctor" $
      Exhaust.renderPat "CONS" 2 `shouldBe` "(CONS _ _)"

  describe "renderCtorWithMissing" $ do
    it "1-arg ctor, pos 0" $
      Exhaust.renderCtorWithMissing "JUST" 1 0 "(NOTHING)" `shouldBe` "(JUST (NOTHING))"

    it "2-arg ctor, pos 1" $
      Exhaust.renderCtorWithMissing "CONS" 2 1 "_" `shouldBe` "(CONS _ _)"

    it "2-arg ctor, pos 0" $
      Exhaust.renderCtorWithMissing "PAIR" 2 0 "X" `shouldBe` "(PAIR X _)"

  describe "patternType" $ do
    it "LitInt → TyInt" $
      Exhaust.patternType (TC.TRPatLit (CST.LitInt 1)) `shouldBe` Ty.TyInt

    it "LitFlt → TyFlt" $
      Exhaust.patternType (TC.TRPatLit (CST.LitFlt 1.0)) `shouldBe` Ty.TyFlt

    it "LitStr → TyStr" $
      Exhaust.patternType (TC.TRPatLit (CST.LitStr "x")) `shouldBe` Ty.TyStr

    it "TRPatBool → TyBool" $
      Exhaust.patternType (TC.TRPatBool True) `shouldBe` Ty.TyBool

    it "TRPatVar → its type" $
      Exhaust.patternType (TC.TRPatVar "X" Ty.TyStr) `shouldBe` Ty.TyStr

    it "TRPatWild → its type" $
      Exhaust.patternType (TC.TRPatWild Ty.TyFlt) `shouldBe` Ty.TyFlt

    it "TRPatCon → its type" $
      Exhaust.patternType (TC.TRPatCon "J" Ty.TyInt []) `shouldBe` Ty.TyInt

  describe "isWildOrVar" $ do
    it "TRPatWild → True" $
      Exhaust.isWildOrVar (TC.TRPatWild Ty.TyInt) `shouldBe` True

    it "TRPatVar → True" $
      Exhaust.isWildOrVar (TC.TRPatVar "X" Ty.TyInt) `shouldBe` True

    it "TRPatBool → False" $
      Exhaust.isWildOrVar (TC.TRPatBool True) `shouldBe` False

    it "TRPatLit → False" $
      Exhaust.isWildOrVar (TC.TRPatLit (CST.LitInt 0)) `shouldBe` False

    it "TRPatCon → False" $
      Exhaust.isWildOrVar (TC.TRPatCon "J" Ty.TyInt []) `shouldBe` False

  describe "isTruePat / isFalsePat" $ do
    it "isTruePat True → True" $
      Exhaust.isTruePat (TC.TRPatBool True) `shouldBe` True

    it "isTruePat False → False" $
      Exhaust.isTruePat (TC.TRPatBool False) `shouldBe` False

    it "isFalsePat False → True" $
      Exhaust.isFalsePat (TC.TRPatBool False) `shouldBe` True

    it "isFalsePat True → False" $
      Exhaust.isFalsePat (TC.TRPatBool True) `shouldBe` False

  describe "missingPatterns" $ do
    it "empty pattern list → [\"_\"]" $
      Exhaust.missingPatterns M.empty Ty.TyInt [] `shouldBe` ["_"]

    it "wildcard alone → []" $
      Exhaust.missingPatterns M.empty Ty.TyInt [TC.TRPatWild Ty.TyInt] `shouldBe` []

    it "variable alone → []" $
      Exhaust.missingPatterns M.empty Ty.TyInt [TC.TRPatVar "X" Ty.TyInt] `shouldBe` []

    it "TyBool only True → [\"FALSE\"]" $
      Exhaust.missingPatterns M.empty Ty.TyBool [TC.TRPatBool True] `shouldBe` ["FALSE"]

    it "TyBool only False → [\"TRUE\"]" $
      Exhaust.missingPatterns M.empty Ty.TyBool [TC.TRPatBool False] `shouldBe` ["TRUE"]

    it "TyBool both → []" $
      Exhaust.missingPatterns M.empty Ty.TyBool [TC.TRPatBool True, TC.TRPatBool False]
        `shouldBe` []

    it "TyInt no wildcard → []" $
      Exhaust.missingPatterns M.empty Ty.TyInt [TC.TRPatLit (CST.LitInt 0)] `shouldBe` []

    it "TyStr no wildcard → []" $
      Exhaust.missingPatterns M.empty Ty.TyStr [TC.TRPatLit (CST.LitStr "x")] `shouldBe` []

    it "TyCon unknown name → []" $
      Exhaust.missingPatterns M.empty (Ty.TyCon "UNKNOWN" [])
        [TC.TRPatCon "UNKNOWN" (Ty.TyCon "UNKNOWN" []) []] `shouldBe` []

    it "TyCon missing ctor → [\"(JUST _)\"]" $ do
      let env = M.fromList [("MAYBE", [("NOTHING", 0), ("JUST", 1)])]
      let pats = [TC.TRPatCon "NOTHING" (Ty.TyCon "MAYBE" []) []]
      Exhaust.missingPatterns env (Ty.TyCon "MAYBE" []) pats `shouldBe` ["(JUST _)"]

    it "TyCon all ctors covered → []" $ do
      let env = M.fromList [("MAYBE", [("NOTHING", 0), ("JUST", 1)])]
      let pats = [ TC.TRPatCon "NOTHING" (Ty.TyCon "MAYBE" []) []
                 , TC.TRPatCon "JUST" (Ty.TyCon "MAYBE" []) [TC.TRPatWild Ty.TyInt] ]
      Exhaust.missingPatterns env (Ty.TyCon "MAYBE" []) pats `shouldBe` []

  describe "full pipeline" $ do
    it "exhaustive case on Maybe → no errors" $ do
      let src = "(type M (a) (N) (J a)) (let ((x (J 1))) (case x ((N) 0) ((J _) 1)))"
      parseCheckExhaust src `shouldBe` []

    it "non-exhaustive: missing J _ → error" $ do
      let src = "(type M (a) (N) (J a)) (let ((x (J 1))) (case x ((N) 0)))"
      parseCheckExhaust src `shouldSatisfy` (not . null)

    it "exhaustive bool both arms → no errors" $ do
      parseCheckExhaust "(let ((b true)) (case b (true 1) (false 0)))" `shouldBe` []

    it "missing FALSE → error" $ do
      parseCheckExhaust "(let ((b true)) (case b (true 1)))"
        `shouldSatisfy` (not . null)

    it "missing TRUE → error" $ do
      parseCheckExhaust "(let ((b true)) (case b (false 0)))"
        `shouldSatisfy` (not . null)

    it "wildcard covers all → no errors" $ do
      parseCheckExhaust "(let ((b true)) (case b (_ 0)))" `shouldBe` []

    it "variable capture covers all → no errors" $ do
      parseCheckExhaust "(let ((b true)) (case b (x 0)))" `shouldBe` []

    it "deep missing: (N) and (J (N)) both missing" $ do
      let src = "(type M (a) (N) (J a)) (let ((x (J (J 1)))) (case x ((J (J _)) 0)))"
      parseCheckExhaust src `shouldSatisfy` (\errs -> length errs >= 2)

    it "case in lambda body propagates" $ do
      let src = "(lam (b) (case b (true 1)))"
      parseCheckExhaust src `shouldSatisfy` (not . null)

    it "case in let body propagates" $ do
      let src = "(let ((b true)) (case b (true 1)))"
      parseCheckExhaust src `shouldSatisfy` (not . null)

    it "case in if branch propagates" $ do
      let src = "(if true (case true (true 1)) 0)"
      parseCheckExhaust src `shouldSatisfy` (not . null)

-- Helper

parseCheckExhaust :: T.Text -> [String]
parseCheckExhaust src = case Parser.parseProgram "<test>" src of
  Left _ -> error "parse error in test"
  Right cst -> case Resolve.resolve cst of
    Left _ -> error "resolve error in test"
    Right resolved -> case TC.typecheck resolved of
      Left _ -> error "typecheck error in test"
      Right typed -> map Exhaust.exhaMsg (Exhaust.exhaustCheck typed)
