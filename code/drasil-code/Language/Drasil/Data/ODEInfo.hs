module Language.Drasil.Data.ODEInfo (
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..)
) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeVarChunk)

-- This may be temporary, but need a structure to hold ODE info for now. 
-- Goal will be for this info to be populated by the instance model for the ODE and the Choices structure.
-- Probably doesn't belong here, but where?
data ODEInfo = ODEInfo {
  indepVar :: CodeVarChunk,
  depVar :: CodeVarChunk,
  otherVars :: [CodeVarChunk],
  tInit :: Expr,
  tFinal :: Expr,
  initVal :: Expr,
  odeSyst :: [Expr],
  odeOpts :: ODEOptions
}

odeInfo :: CodeVarChunk -> CodeVarChunk -> [CodeVarChunk] -> Expr -> Expr -> 
  Expr -> [Expr] -> ODEOptions -> ODEInfo
odeInfo = ODEInfo

data ODEOptions = ODEOpts {
  solveMethod :: ODEMethod,
  absTol :: Expr,
  relTol :: Expr,
  stepSize :: Expr
}

odeOptions :: ODEMethod -> Expr -> Expr -> Expr -> ODEOptions
odeOptions = ODEOpts

data ODEMethod = RK45 | BDF | Adams
