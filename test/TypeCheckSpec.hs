{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.TypeCheck as TC

spec :: Spec
spec = do
  describe "error collection" $ do
    it "collects multiple type mismatch errors" $ do
      pending

    it "collects errors from unifyMany" $ do
      pending

    it "collects errors from both inference and solving" $ do
      pending

  describe "valid programs" $ do
    it "typechecks arithmetic expressions" $ do
      pending

    it "typechecks let bindings" $ do
      pending

    it "typechecks if expressions" $ do
      pending

-- Helper to parse, resolve, and typecheck
parseAndTypecheck :: T.Text -> Either [TC.TypeError] TC.TResolvedCST
parseAndTypecheck src = case Parser.parseProgram src of
  Left _ -> error "parse error in test"
  Right cst -> case Resolve.resolve cst of
    Left _ -> error "resolve error in test"
    Right resolved -> TC.typecheck resolved
