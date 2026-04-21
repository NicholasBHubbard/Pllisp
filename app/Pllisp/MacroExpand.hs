{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.MacroExpand (expand) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.SrcLoc as Loc

-- CORE TYPES

data MacroClause = MacroClause
  { mcParams   :: [MacroParam]
  , mcTemplate :: SExpr.SExpr
  , mcDoc      :: Maybe T.Text
  } deriving (Show)

data MacroParam
  = ParamSingle T.Text
  | ParamRest T.Text
  | ParamDestructure [MacroParam]
  deriving (Show)

type MacroTable = M.Map T.Text [MacroClause]
type Bindings   = M.Map T.Text SExpr.SExpr

maxDepth :: Int
maxDepth = 256

-- ENTRY POINT

expand :: [SExpr.SExpr] -> Either String [SExpr.SExpr]
expand sexprs = do
  let (macros, rest) = collectMacros M.empty sexprs
  mapM (expandExpr macros 0) rest

-- COLLECT MACRO DEFINITIONS

-- Walk top-level sexprs, collect (mac ...) forms into the table,
-- return remaining non-macro sexprs.
collectMacros :: MacroTable -> [SExpr.SExpr] -> (MacroTable, [SExpr.SExpr])
collectMacros table [] = (table, [])
collectMacros table (sx : rest) = case Loc.locVal sx of
  SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : macRest) ->
    case parseMacDef macRest of
      Right (name, clause) ->
        let existing = M.findWithDefault [] name table
            table'   = M.insert name (existing ++ [clause]) table
        in collectMacros table' rest
      Left _ -> collectMacros table rest
  _ -> let (table', rest') = collectMacros table rest
       in (table', sx : rest')

parseMacDef :: [SExpr.SExpr] -> Either String (T.Text, MacroClause)
parseMacDef [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SStr doc), Loc.Located _ (SExpr.SList paramSxs), template] = do
  params <- parseParams paramSxs
  Right (name, MacroClause params template (Just doc))
parseMacDef [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SList paramSxs), template] = do
  params <- parseParams paramSxs
  Right (name, MacroClause params template Nothing)
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

expandExpr :: MacroTable -> Int -> SExpr.SExpr -> Either String SExpr.SExpr
expandExpr macros depth sx
  | depth > maxDepth = Left "macro expansion depth limit exceeded"
  | otherwise = case Loc.locVal sx of
      SExpr.SList (Loc.Located _ (SExpr.SAtom name) : args)
        | Just clauses <- M.lookup name macros -> do
            expanded <- applyMacro (Loc.locSpan sx) clauses args name
            expandExpr macros (depth + 1) expanded
      SExpr.SList elems -> do
        elems' <- mapM (expandExpr macros depth) elems
        pure (Loc.Located (Loc.locSpan sx) (SExpr.SList elems'))
      _ -> pure sx

applyMacro :: Loc.Span -> [MacroClause] -> [SExpr.SExpr] -> T.Text -> Either String SExpr.SExpr
applyMacro _ [] args name =
  Left ("no matching clause for macro " ++ T.unpack name
    ++ " with " ++ show (length args) ++ " argument(s)")
applyMacro sp (clause : rest) args name =
  case matchClause clause args of
    Just bindings -> instantiate sp bindings (mcTemplate clause)
    Nothing       -> applyMacro sp rest args name

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

-- TEMPLATE INSTANTIATION

instantiate :: Loc.Span -> Bindings -> SExpr.SExpr -> Either String SExpr.SExpr
instantiate callSpan binds sx = case Loc.locVal sx of
  -- Quasiquote: enter template mode
  SExpr.SQuasi inner -> evalQuasi callSpan binds inner
  -- Bare param reference (outside quasiquote): substitute
  SExpr.SAtom name
    | Just val <- M.lookup name binds -> Right val
  -- Everything else: return as-is
  _ -> Right sx

evalQuasi :: Loc.Span -> Bindings -> SExpr.SExpr -> Either String SExpr.SExpr
evalQuasi callSpan binds sx = case Loc.locVal sx of
  -- ,x — unquote: substitute binding
  SExpr.SUnquote inner -> case Loc.locVal inner of
    SExpr.SAtom name
      | Just val <- M.lookup name binds -> Right val
      | otherwise -> Right inner
    _ -> Right inner

  -- List with possible ,@ splicing
  SExpr.SList elems -> do
    elems' <- evalQuasiList callSpan binds elems
    Right (Loc.Located callSpan (SExpr.SList elems'))

  -- Nested quasiquote — pass through (no nested quasiquote support)
  SExpr.SQuasi _ -> Right sx

  -- Everything else is literal in quasiquote context
  _ -> Right sx

-- Process list elements, handling ,@ splicing
evalQuasiList :: Loc.Span -> Bindings -> [SExpr.SExpr] -> Either String [SExpr.SExpr]
evalQuasiList _ _ [] = Right []
evalQuasiList callSpan binds (sx : rest) = case Loc.locVal sx of
  -- ,@x — splice: insert list elements inline
  SExpr.SSplice inner -> case Loc.locVal inner of
    SExpr.SAtom name
      | Just val <- M.lookup name binds -> case Loc.locVal val of
          SExpr.SList spliced -> do
            rest' <- evalQuasiList callSpan binds rest
            Right (spliced ++ rest')
          _ -> Left (",@ on non-list value for " ++ T.unpack name)
      | otherwise -> Left ("unbound splice variable: " ++ T.unpack name)
    _ -> Left ",@ requires a variable"
  -- Regular element: recurse into it
  _ -> do
    sx'   <- evalQuasi callSpan binds sx
    rest' <- evalQuasiList callSpan binds rest
    Right (sx' : rest')

-- HELPERS

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)
