{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.MacroExpand (expand, expandWith, extractMacroDefs) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Control.Monad.State.Strict as State

import qualified Pllisp.MacroInterp as MI
import qualified Pllisp.SExpr       as SExpr
import qualified Pllisp.SrcLoc      as Loc

-- CORE TYPES

data MacroClause = MacroClause
  { mcParams   :: [MacroParam]
  , mcTemplate :: SExpr.SExpr
  } deriving (Show)

data MacroParam
  = ParamSingle T.Text
  | ParamRest T.Text
  | ParamDestructure [MacroParam]
  deriving (Show)

type MacroTable = M.Map T.Text MacroClause
type Bindings   = M.Map T.Text SExpr.SExpr

-- ExpandM is the same monad as InterpM (StateT Int over Either String),
-- so MI.eval can be called directly and shares the gensym counter.
type ExpandM = State.StateT Int (Either String)

throwExpandError :: String -> ExpandM a
throwExpandError = State.StateT . const . Left

maxDepth :: Int
maxDepth = 256

-- ENTRY POINT

expand :: [SExpr.SExpr] -> Either String [SExpr.SExpr]
expand = expandWith []

-- | Expand with imported macro sexprs pre-seeded (not subject to duplicate check).
expandWith :: [SExpr.SExpr] -> [SExpr.SExpr] -> Either String [SExpr.SExpr]
expandWith importedMacros localSexprs = do
  (baseTable, _) <- collectMacros M.empty importedMacros
  (macros, rest) <- collectMacros baseTable localSexprs
  State.evalStateT (mapM (expandExpr macros 0) rest) 0

-- COLLECT MACRO DEFINITIONS

-- Walk top-level sexprs, collect (mac ...) forms into the table,
-- return remaining non-macro sexprs.
collectMacros :: MacroTable -> [SExpr.SExpr] -> Either String (MacroTable, [SExpr.SExpr])
collectMacros table [] = Right (table, [])
collectMacros table (sx : rest) = case Loc.locVal sx of
  SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : macRest) ->
    case parseMacDef macRest of
      Right (name, clause)
        | M.member name table ->
            Left ("duplicate macro definition: " ++ T.unpack name)
        | otherwise ->
            collectMacros (M.insert name clause table) rest
      Left err -> Left err
  _ -> do (table', rest') <- collectMacros table rest
          Right (table', sx : rest')

parseMacDef :: [SExpr.SExpr] -> Either String (T.Text, MacroClause)
parseMacDef [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SList paramSxs), template] = do
  params <- parseParams paramSxs
  Right (name, MacroClause params template)
parseMacDef _ = Left "invalid mac definition"

parseParams :: [SExpr.SExpr] -> Either String [MacroParam]
parseParams [] = Right []
parseParams [Loc.Located _ (SExpr.SAtom "&REST"), Loc.Located _ (SExpr.SAtom name)] =
  Right [ParamRest name]
parseParams (Loc.Located _ (SExpr.SAtom "&REST") : _) =
  Left "&rest must be followed by exactly one name at end of param list"
parseParams (Loc.Located _ (SExpr.SAtom name) : rest) = do
  rest' <- parseParams rest
  Right (ParamSingle name : rest')
parseParams (Loc.Located _ (SExpr.SList subParams) : rest) = do
  sub  <- parseParams subParams
  rest' <- parseParams rest
  Right (ParamDestructure sub : rest')
parseParams _ = Left "invalid macro parameter"

-- EXPANSION

expandExpr :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandExpr macros depth sx
  | depth > maxDepth = throwExpandError "macro expansion depth limit exceeded"
  | otherwise = case Loc.locVal sx of
      SExpr.SList (Loc.Located _ (SExpr.SAtom name) : args)
        | Just clause <- M.lookup name macros -> do
            expanded <- applyMacro clause args name
            expandExpr macros (depth + 1) expanded
      SExpr.SList elems -> do
        elems' <- mapM (expandExpr macros depth) elems
        pure (Loc.Located (Loc.locSpan sx) (SExpr.SList elems'))
      _ -> pure sx

applyMacro :: MacroClause -> [SExpr.SExpr] -> T.Text -> ExpandM SExpr.SExpr
applyMacro clause args name =
  case matchClause clause args of
    Just bindings -> do
      let mvalBinds = M.map MI.sexprToVal bindings
          env = M.union mvalBinds MI.defaultEnv
      result <- MI.eval env (mcTemplate clause)
      case MI.valToSExpr result of
        Left err -> throwExpandError err
        Right sexpr -> pure sexpr
    Nothing ->
      throwExpandError ("macro " ++ T.unpack name
        ++ " does not accept " ++ show (length args) ++ " argument(s)")

-- CLAUSE MATCHING

matchClause :: MacroClause -> [SExpr.SExpr] -> Maybe Bindings
matchClause clause args = go (mcParams clause) args M.empty
  where
    go [] [] binds = Just binds
    go [] _  _     = Nothing
    go (ParamRest name : _) restArgs binds =
      Just (M.insert name (Loc.Located dummySpan (SExpr.SList restArgs)) binds)
    go (ParamSingle name : ps) (a : as) binds =
      go ps as (M.insert name a binds)
    go (ParamDestructure subPs : ps) (a : as) binds =
      case Loc.locVal a of
        SExpr.SList subArgs -> case go subPs subArgs binds of
          Just binds' -> go ps as binds'
          Nothing     -> Nothing
        _ -> Nothing
    go _ _ _ = Nothing

-- MACRO EXTRACTION

-- | Extract (mac ...) forms from a list of SExprs.
-- Used to collect macro definitions from imported modules.
extractMacroDefs :: [SExpr.SExpr] -> [SExpr.SExpr]
extractMacroDefs = filter isMacDef
  where
    isMacDef (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : _))) = True
    isMacDef _ = False

-- HELPERS

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
