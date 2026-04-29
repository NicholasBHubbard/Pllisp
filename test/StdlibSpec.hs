{-# LANGUAGE OverloadedStrings #-}

module StdlibSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import qualified Pllisp.Parser as Parser
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc

spec :: Spec
spec = do
  describe "PRELUDE style" $ do
    it "uses fun instead of raw let/lambda wrappers for named function definitions" $ do
      src <- T.IO.readFile "stdlib/PRELUDE.pll"
      sexprs <- case Parser.parseSExprs "stdlib/PRELUDE.pll" src of
        Left err -> expectationFailure ("parse error: " ++ show err) >> pure []
        Right parsed -> pure parsed
      collectWrappedNames sexprs `shouldBe` []

collectWrappedNames :: [SExpr.SExpr] -> [T.Text]
collectWrappedNames = concatMap go
  where
    go sx = case Loc.locVal sx of
      SExpr.SList (Loc.Located _ (SExpr.SAtom "EVAL-WHEN") : _phaseSx : bodyForms) ->
        collectWrappedNames bodyForms
      _ ->
        case wrappedName sx of
          Just name -> [name]
          Nothing -> []

wrappedName :: SExpr.SExpr -> Maybe T.Text
wrappedName (Loc.Located _
  (SExpr.SList
    [ Loc.Located _ (SExpr.SAtom "LET")
    , Loc.Located _ (SExpr.SList
        [ Loc.Located _ (SExpr.SList
            [ Loc.Located _ (SExpr.SAtom bindName)
            , Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "LAM") : _))
            ])
        ])
    , Loc.Located _ (SExpr.SAtom bodyName)
    ]))
  | bindName == bodyName = Just bindName
wrappedName _ = Nothing
