{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.LambdaLift where

import qualified Pllisp.CST as CST
import qualified Pllisp.Type as Ty
import qualified Pllisp.ClosureConvert as CC

import Control.Monad.State.Strict
import qualified Data.Text as T

-- ENTRY POINT

lambdaLift :: CC.CCProgram -> LLProgram
lambdaLift ccExprs =
  let (exprs, finalState) = runState (mapM liftExpr ccExprs) (LiftState 0 [])
  in LLProgram
    { llDefs  = reverse (lsDefs finalState)
    , llExprs = exprs
    }

-- CORE

data LLProgram = LLProgram
  { llDefs  :: [LLDef]
  , llExprs :: [LLExpr]
  } deriving (Eq, Show)

data LLDef = LLDef
  { defName   :: CST.Symbol
  , defEnv    :: [(CST.Symbol, Ty.Type)]
  , defParams :: [(CST.Symbol, Ty.Type)]
  , defRetTy  :: Ty.Type
  , defBody   :: LLExpr
  } deriving (Eq, Show)

type LLExpr = Ty.Typed LLExprF

data LLExprF
  = LLVar  CST.Symbol Ty.Type
  | LLLit  CST.Literal
  | LLBool Bool
  | LLUnit
  | LLLet  [(CST.Symbol, Ty.Type, LLExpr)] LLExpr
  | LLIf   LLExpr LLExpr LLExpr
  | LLApp  LLExpr [LLExpr]
  | LLType CST.Symbol [CST.Symbol] [CST.DataCon]
  | LLFFI  CST.Symbol [Ty.CType] Ty.CType
  | LLFFIStruct CST.Symbol [(CST.Symbol, Ty.CType)]
  | LLFFIVar CST.Symbol [Ty.CType] Ty.CType
  | LLFFIEnum CST.Symbol [(CST.Symbol, Integer)]
  | LLFFICallback CST.Symbol [Ty.CType] Ty.CType
  | LLCase LLExpr [(LLPattern, LLExpr)]
  | LLMkClosure CST.Symbol [LLExpr]
  | LLLoop [(CST.Symbol, Ty.Type)] LLExpr
  | LLRecur [LLExpr]
  deriving (Eq, Show)

data LLPattern
  = LLPatLit  CST.Literal
  | LLPatBool Bool
  | LLPatVar  CST.Symbol Ty.Type
  | LLPatWild Ty.Type
  | LLPatCon  CST.Symbol Ty.Type [LLPattern]
  deriving (Eq, Show)

-- LIFT MONAD

data LiftState = LiftState
  { lsCounter :: !Int
  , lsDefs    :: [LLDef]
  }

type LiftM = State LiftState

freshName :: LiftM CST.Symbol
freshName = do
  n <- gets lsCounter
  modify' (\s -> s { lsCounter = n + 1 })
  pure ("__lambda_" <> T.pack (show n))

emitDef :: LLDef -> LiftM ()
emitDef d = modify' (\s -> s { lsDefs = d : lsDefs s })

-- CONVERSION

liftExpr :: CC.CCExpr -> LiftM LLExpr
liftExpr (Ty.Typed t expr) = case expr of
  CC.CCLam params freeVars retTy body -> do
    body' <- liftExpr body
    name  <- freshName
    emitDef LLDef
      { defName   = name
      , defEnv    = freeVars
      , defParams = params
      , defRetTy  = retTy
      , defBody   = body'
      }
    let fvExprs = [Ty.Typed fvTy (LLVar fvName fvTy) | (fvName, fvTy) <- freeVars]
    pure (Ty.Typed t (LLMkClosure name fvExprs))

  CC.CCLit l    -> pure (Ty.Typed t (LLLit l))
  CC.CCBool b   -> pure (Ty.Typed t (LLBool b))
  CC.CCUnit     -> pure (Ty.Typed t LLUnit)
  CC.CCVar s vt -> pure (Ty.Typed t (LLVar s vt))

  CC.CCLet binds body -> do
    binds' <- mapM (\(n, bt, rhs) -> do rhs' <- liftExpr rhs; pure (n, bt, rhs')) binds
    body' <- liftExpr body
    pure (Ty.Typed t (LLLet binds' body'))

  CC.CCIf c th el -> do
    c'  <- liftExpr c
    th' <- liftExpr th
    el' <- liftExpr el
    pure (Ty.Typed t (LLIf c' th' el'))

  CC.CCApp f args -> do
    f'    <- liftExpr f
    args' <- mapM liftExpr args
    pure (Ty.Typed t (LLApp f' args'))

  CC.CCType n ps cs -> pure (Ty.Typed t (LLType n ps cs))
  CC.CCFFI n pts rt -> pure (Ty.Typed t (LLFFI n pts rt))
  CC.CCFFIStruct n fs -> pure (Ty.Typed t (LLFFIStruct n fs))
  CC.CCFFIVar n pts rt -> pure (Ty.Typed t (LLFFIVar n pts rt))
  CC.CCFFIEnum n vs -> pure (Ty.Typed t (LLFFIEnum n vs))
  CC.CCFFICallback n pts rt -> pure (Ty.Typed t (LLFFICallback n pts rt))

  CC.CCCase scr arms -> do
    scr' <- liftExpr scr
    arms' <- mapM (\(p, b) -> do b' <- liftExpr b; pure (liftPattern p, b')) arms
    pure (Ty.Typed t (LLCase scr' arms'))

  CC.CCLoop params body -> do
    body' <- liftExpr body
    pure (Ty.Typed t (LLLoop params body'))

  CC.CCRecur args -> do
    args' <- mapM liftExpr args
    pure (Ty.Typed t (LLRecur args'))

liftPattern :: CC.CCPattern -> LLPattern
liftPattern pat = case pat of
  CC.CCPatLit l      -> LLPatLit l
  CC.CCPatBool b     -> LLPatBool b
  CC.CCPatVar s t    -> LLPatVar s t
  CC.CCPatWild t     -> LLPatWild t
  CC.CCPatCon c t ps -> LLPatCon c t (map liftPattern ps)
