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
    it "matches the reduced primitive runtime surface" $
      BuiltIn.builtInNames `shouldBe` S.fromList
        [ "ADD", "SUB", "MUL", "DIV", "MOD"
        , "ADDF", "SUBF", "MULF", "DIVF"
        , "EQI", "LTI"
        , "EQF", "LTF"
        , "EQS", "LTS"
        , "CONCAT", "STRLEN", "SUBSTR"
        , "INT-TO-FLT", "FLT-TO-INT"
        , "USYM-TO-STR", "STR-TO-USYM"
        , "REF", "DEREF", "SET!"
        ]

    it "does not keep convenience runtime built-ins in scope" $ do
      mapM_ (\name -> S.member name BuiltIn.builtInNames `shouldBe` False)
        [ "NEG", "NEGF"
        , "GTI", "LEI", "GEI"
        , "GTF", "LEF", "GEF"
        , "GTS", "LES", "GES"
        , "AND", "OR", "NOT"
        , "STR-CONTAINS"
        , "PRINT", "READ-LINE", "IS-EOF"
        , "ARGC", "ARGV"
        , "INT-TO-STR", "FLT-TO-STR"
        , "RX-MATCH", "RX-FIND", "RX-SUB", "RX-GSUB"
        , "RX-SPLIT", "RX-CAPTURES", "RX-COMPILE"
        , "GC-COLLECT", "GC-HEAP-SIZE"
        ]

  describe "builtInTypes" $ do
    it "ADD :: Int -> Int -> Int" $
      M.lookup "ADD" BuiltIn.builtInTypes
        `shouldBe` Just (Ty.TyFun [Ty.TyInt, Ty.TyInt] Ty.TyInt)

    it "NEG is not a primitive builtin anymore" $
      M.lookup "NEG" BuiltIn.builtInTypes `shouldBe` Nothing

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
