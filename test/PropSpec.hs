{-# LANGUAGE OverloadedStrings #-}

module PropSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Pllisp.CST      as CST
import qualified Pllisp.ExhaustCheck as Exhaust
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC

-- ARBITRARY INSTANCES

newtype ArbType = ArbType Ty.Type deriving (Show)

instance Arbitrary ArbType where
  -- limit depth to avoid exponential blowup
  arbitrary = ArbType <$> genType 3

genType :: Int -> Gen Ty.Type
genType 0 = elements [Ty.TyInt, Ty.TyFlt, Ty.TyStr, Ty.TyBool]
genType n = frequency
  [ (4, elements [Ty.TyInt, Ty.TyFlt, Ty.TyStr, Ty.TyBool])
  , (2, Ty.TyVar <$> choose (0, 5))
  , (1, Ty.TyFun <$> vectorOf 2 (genType (n - 1)) <*> genType (n - 1))
  , (1, Ty.TyCon <$> elements ["MAYBE", "LIST", "EITHER"] <*> vectorOf 1 (genType (n - 1)))
  ]

newtype ArbLiteral = ArbLiteral CST.Literal deriving (Show)

instance Arbitrary ArbLiteral where
  arbitrary = ArbLiteral <$> oneof
    [ CST.LitInt <$> arbitrary
    , CST.LitFlt <$> arbitrary
    , CST.LitStr . T.pack <$> listOf (elements ['a'..'z'])
    ]

-- PROPERTIES

prop_renderType_percent :: ArbType -> Bool
prop_renderType_percent (ArbType t) =
  "%" `T.isPrefixOf` Ty.renderType t

prop_renderType_funArity :: ArbType -> [ArbType] -> Property
prop_renderType_funArity (ArbType ret) rawArgs =
  let args = take 4 rawArgs  -- bound to avoid huge strings
  in not (null args) ==>
    let argTys  = map (\(ArbType a) -> a) args
        rendered = Ty.renderType (Ty.TyFun argTys ret)
    in all (\a -> T.unpack (Ty.renderType a) `L.isInfixOf` T.unpack rendered) argTys

prop_renderPat_zero :: String -> Bool
prop_renderPat_zero ctorStr =
  let ctor = T.pack ctorStr
  in Exhaust.renderPat ctor 0 == "(" ++ ctorStr ++ ")"

-- Limit n to small values; ctor must not contain " _" to avoid false counts
prop_renderPat_n :: Property
prop_renderPat_n =
  forAll (listOf (elements ['A'..'Z'])) $ \ctorStr ->
  forAll (choose (0, 10)) $ \n ->
    let ctor   = T.pack ctorStr
        result = Exhaust.renderPat ctor n
        occurrences = length (filter (L.isPrefixOf " _") (L.tails result))
    in occurrences == n

prop_isWildOrVar_wild :: ArbType -> Bool
prop_isWildOrVar_wild (ArbType t) =
  Exhaust.isWildOrVar (TC.TRPatWild t)

prop_isWildOrVar_var :: ArbType -> Bool
prop_isWildOrVar_var (ArbType t) =
  Exhaust.isWildOrVar (TC.TRPatVar "X" t)

prop_isWildOrVar_lit :: ArbLiteral -> Bool
prop_isWildOrVar_lit (ArbLiteral l) =
  not (Exhaust.isWildOrVar (TC.TRPatLit l))

prop_missing_empty :: ArbType -> Bool
prop_missing_empty (ArbType t) =
  not (null (Exhaust.missingPatterns M.empty t []))

prop_missing_wild :: ArbType -> Bool
prop_missing_wild (ArbType t) =
  null (Exhaust.missingPatterns M.empty t [TC.TRPatWild t])

prop_missing_var :: ArbType -> Bool
prop_missing_var (ArbType t) =
  null (Exhaust.missingPatterns M.empty t [TC.TRPatVar "X" t])

prop_renderCtorWithMissing_starts_paren :: String -> String -> Property
prop_renderCtorWithMissing_starts_paren ctorStr sub =
  forAll (choose (1, 8)) $ \arity ->
  forAll (choose (0, arity - 1)) $ \pos ->
    let ctor = T.pack ctorStr
    in "(" `L.isPrefixOf` Exhaust.renderCtorWithMissing ctor arity pos sub

-- SPEC

spec :: Spec
spec = do
  describe "properties" $ do
    it "renderType always starts with %" $
      property prop_renderType_percent

    it "renderType TyFun contains each arg's renderType" $
      property prop_renderType_funArity

    it "renderPat 0-arg = (CTOR)" $
      property prop_renderPat_zero

    it "renderPat n-arg has exactly n occurrences of \" _\"" $
      property prop_renderPat_n

    it "isWildOrVar TRPatWild = True" $
      property prop_isWildOrVar_wild

    it "isWildOrVar TRPatVar = True" $
      property prop_isWildOrVar_var

    it "isWildOrVar TRPatLit = False" $
      property prop_isWildOrVar_lit

    it "missingPatterns [] is non-empty" $
      property prop_missing_empty

    it "missingPatterns [TRPatWild] = []" $
      property prop_missing_wild

    it "missingPatterns [TRPatVar] = []" $
      property prop_missing_var

    it "renderCtorWithMissing always starts with \"(\"" $
      property prop_renderCtorWithMissing_starts_paren
