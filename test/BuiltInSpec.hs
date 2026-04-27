{-# LANGUAGE OverloadedStrings #-}

module BuiltInSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Pllisp.BuiltIn as BuiltIn
import qualified Pllisp.Type as Ty

spec :: Spec
spec = do
  describe "builtInNames" $ do
    it "has 52 entries" $
      S.size BuiltIn.builtInNames `shouldBe` 52

    it "contains ADD, NOT, PRINT, FLT-TO-STR, EQF" $ do
      S.member "ADD"      BuiltIn.builtInNames `shouldBe` True
      S.member "NOT"      BuiltIn.builtInNames `shouldBe` True
      S.member "PRINT"    BuiltIn.builtInNames `shouldBe` True
      S.member "FLT-TO-STR" BuiltIn.builtInNames `shouldBe` True
      S.member "EQF"      BuiltIn.builtInNames `shouldBe` True

    it "contains REF, DEREF, SET!" $ do
      S.member "REF"   BuiltIn.builtInNames `shouldBe` True
      S.member "DEREF" BuiltIn.builtInNames `shouldBe` True
      S.member "SET!"  BuiltIn.builtInNames `shouldBe` True

  describe "builtInTypes" $ do
    it "ADD :: Int -> Int -> Int" $
      M.lookup "ADD" BuiltIn.builtInTypes
        `shouldBe` Just (Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)

    it "NEG :: Int -> Int" $
      M.lookup "NEG" BuiltIn.builtInTypes
        `shouldBe` Just (Ty.TyFun [Ty.TyInt] Ty.TyInt)

  describe "builtInSchemes" $ do
    it "monomorphic built-ins have empty forall" $ do
      let check name = case M.lookup name BuiltIn.builtInSchemes of
            Nothing     -> expectationFailure ("missing: " ++ show name)
            Just (vs, _) -> S.null vs `shouldBe` True
      mapM_ check (S.toList (M.keysSet BuiltIn.builtInTypes))

    it "REF, DEREF, SET! are polymorphic" $ do
      let check name = case M.lookup name BuiltIn.builtInSchemes of
            Nothing     -> expectationFailure ("missing: " ++ show name)
            Just (vs, _) -> S.null vs `shouldBe` False
      mapM_ check ["REF", "DEREF", "SET!"]
