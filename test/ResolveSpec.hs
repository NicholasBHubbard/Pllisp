{-# LANGUAGE OverloadedStrings #-}

module ResolveSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Resolve as Resolve

spec :: Spec
spec = do
  describe "error collection" $ do
    it "collects multiple undefined symbol errors" $ do
      pending

    it "collects duplicate binding errors" $ do
      pending

    it "collects errors from nested scopes" $ do
      pending

  describe "valid programs" $ do
    it "resolves simple let bindings" $ do
      pending

    it "resolves lambda parameters" $ do
      pending

-- Helper to parse and resolve
parseAndResolve :: T.Text -> Either [Resolve.ResolveError] Resolve.ResolvedCST
parseAndResolve src = case Parser.parseProgram "<test>" src of
  Left _    -> error "parse error in test"
  Right cst -> Resolve.resolve cst
