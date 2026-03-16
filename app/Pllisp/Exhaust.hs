-- MODULE

module Pllisp.Exhaust where

import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- CORE

-- | Maps each type name to its constructors and their arities
type CtorEnv = M.Map CST.Symbol [(CST.Symbol, Int)]

data ExhaustError = ExhaustError
  { exhaSpan :: Loc.Span
  , exhaMsg  :: String
  } deriving (Show)

exhaustCheck :: TC.TResolvedCST -> [ExhaustError]
exhaustCheck typed =
  let env = buildCtorEnv typed
  in concatMap (checkExpr env) typed

buildCtorEnv :: TC.TResolvedCST -> CtorEnv
buildCtorEnv = M.fromList . concatMap go
  where
    go (Loc.Located _ (Ty.Typed _ (TC.TRType name _ ctors))) =
      [(name, map (\dc -> (CST.dcName dc, length (CST.dcArgs dc))) ctors)]
    go _ = []

-- EXPRESSION WALK

checkExpr :: CtorEnv -> TC.TRExpr -> [ExhaustError]
checkExpr env (Loc.Located sp (Ty.Typed _ expr)) = case expr of
  TC.TRLit _       -> []
  TC.TRBool _      -> []
  TC.TRUnit        -> []
  TC.TRVar _       -> []
  TC.TRType _ _ _  -> []
  TC.TRLam _ _ body ->
    checkExpr env body
  TC.TRLet binds body ->
    concatMap (\(_, _, e) -> checkExpr env e) binds ++ checkExpr env body
  TC.TRIf c t e ->
    checkExpr env c ++ checkExpr env t ++ checkExpr env e
  TC.TRApp f as ->
    checkExpr env f ++ concatMap (checkExpr env) as
  TC.TRCase scr arms ->
    let scrutTy  = TC.typeOf scr
        topPats  = map fst arms
        missing  = missingPatterns env scrutTy topPats
        caseErrs = map (\m -> ExhaustError sp
                    ("non-exhaustive patterns in case expression, missing: " ++ m)) missing
        scrErrs  = checkExpr env scr
        armErrs  = concatMap (\(_, body) -> checkExpr env body) arms
    in caseErrs ++ scrErrs ++ armErrs

-- EXHAUSTIVENESS

-- | Returns a list of example missing patterns (empty = exhaustive).
missingPatterns :: CtorEnv -> Ty.Type -> [TC.TRPattern] -> [String]
missingPatterns _ _ [] = ["_"]
missingPatterns env ty pats
  | any isWildOrVar pats = []
  | otherwise = case ty of
      Ty.TyBool ->
        (if not (any isTruePat  pats) then ["TRUE"]  else []) ++
        (if not (any isFalsePat pats) then ["FALSE"] else [])
      Ty.TyCon name _ ->
        case M.lookup name env of
          Nothing    -> []
          Just ctors -> concatMap (missingForCtor env pats) ctors
      _ -> []   -- TyInt / TyFlt / TyStr: infinite domain; only wildcards cover

-- | Find missing patterns for a single constructor.
missingForCtor :: CtorEnv -> [TC.TRPattern] -> (CST.Symbol, Int) -> [String]
missingForCtor env pats (ctor, arity) =
  let matchingRows = [subpats | TC.TRPatCon c _ subpats <- pats, c == ctor]
  in if null matchingRows
     then [renderPat ctor arity]          -- constructor never mentioned
     else if arity == 0
          then []                         -- 0-arg constructor: presence = exhaustive
          else concatMap (checkArgPos env ctor arity matchingRows) [0..arity-1]

-- | Check one argument position across all matching arms for a constructor.
checkArgPos :: CtorEnv -> CST.Symbol -> Int -> [[TC.TRPattern]] -> Int -> [String]
checkArgPos env ctor arity rows i =
  let patsForPos = map (!! i) rows
  in case patsForPos of
       []    -> []
       (p:_) ->
         let subMissing = missingPatterns env (patternType p) patsForPos
         in map (renderCtorWithMissing ctor arity i) subMissing

-- HELPERS

patternType :: TC.TRPattern -> Ty.Type
patternType (TC.TRPatLit (CST.LitInt _)) = Ty.TyInt
patternType (TC.TRPatLit (CST.LitFlt _)) = Ty.TyFlt
patternType (TC.TRPatLit (CST.LitStr _)) = Ty.TyStr
patternType (TC.TRPatBool _)             = Ty.TyBool
patternType (TC.TRPatVar _ t)            = t
patternType (TC.TRPatWild t)             = t
patternType (TC.TRPatCon _ t _)          = t

isWildOrVar :: TC.TRPattern -> Bool
isWildOrVar (TC.TRPatWild _)  = True
isWildOrVar (TC.TRPatVar _ _) = True
isWildOrVar _                 = False

isTruePat :: TC.TRPattern -> Bool
isTruePat (TC.TRPatBool True) = True
isTruePat _                   = False

isFalsePat :: TC.TRPattern -> Bool
isFalsePat (TC.TRPatBool False) = True
isFalsePat _                    = False

-- | Render a fully-wildcarded pattern for a missing constructor.
--   renderPat "NOTHING" 0  =  "(NOTHING)"
--   renderPat "JUST" 1     =  "(JUST _)"
renderPat :: CST.Symbol -> Int -> String
renderPat ctor arity =
  "(" ++ T.unpack ctor ++ concat (replicate arity " _") ++ ")"

-- | Render a constructor pattern where position `missingPos` contains `subPat`
--   and all other positions are wildcards.
--   renderCtorWithMissing "JUST" 1 0 "(NOTHING)"  =  "(JUST (NOTHING))"
renderCtorWithMissing :: CST.Symbol -> Int -> Int -> String -> String
renderCtorWithMissing ctor arity missingPos subPat =
  let args = [if i == missingPos then " " ++ subPat else " _" | i <- [0..arity-1]]
  in "(" ++ T.unpack ctor ++ concat args ++ ")"
