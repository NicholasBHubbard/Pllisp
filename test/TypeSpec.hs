{-# LANGUAGE OverloadedStrings #-}

module TypeSpec (spec) where

import Test.Hspec

import qualified Pllisp.Type as Ty

spec :: Spec
spec = do
  describe "renderType" $ do
    it "TyInt" $
      Ty.renderType Ty.TyInt `shouldBe` "%INT"

    it "TyFlt" $
      Ty.renderType Ty.TyFlt `shouldBe` "%FLT"

    it "TyStr" $
      Ty.renderType Ty.TyStr `shouldBe` "%STR"

    it "TyBool" $
      Ty.renderType Ty.TyBool `shouldBe` "%BOOL"

    it "TyUnit" $
      Ty.renderType Ty.TyUnit `shouldBe` "%UNIT"

    it "TyFun 1 arg" $
      Ty.renderType (Ty.TyFun [Ty.TyInt] Ty.TyBool) `shouldBe` "%(-> %INT %BOOL)"

    it "TyFun 2 args" $
      Ty.renderType (Ty.TyFun [Ty.TyInt, Ty.TyStr] Ty.TyBool) `shouldBe` "%(-> %INT %STR %BOOL)"

    it "TyCon no args" $
      Ty.renderType (Ty.TyCon "MAYBE" []) `shouldBe` "%MAYBE"

    it "TyCon 1 arg" $
      Ty.renderType (Ty.TyCon "MAYBE" [Ty.TyInt]) `shouldBe` "%(MAYBE %INT)"

    it "TyVar 0" $
      Ty.renderType (Ty.TyVar 0) `shouldBe` "%t0"

    it "nested TyFun containing TyCon" $
      Ty.renderType (Ty.TyFun [Ty.TyCon "MAYBE" [Ty.TyVar 0]] Ty.TyInt)
        `shouldBe` "%(-> %(MAYBE %t0) %INT)"
