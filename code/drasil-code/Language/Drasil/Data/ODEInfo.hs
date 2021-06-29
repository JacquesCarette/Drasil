-- | Defines a structure to contain scientifically-relevant information about
-- an ODE.
module Language.Drasil.Data.ODEInfo (
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..)
) where

import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.CodeExpr (CodeExpr)


-- This may be temporary, but need a structure to hold ODE info for now. 
-- Goal will be for this info to be populated by the instance model for the ODE and the Choices structure.
-- Probably doesn't belong here, but where?
data ODEInfo = ODEInfo {
  indepVar :: CodeVarChunk,
  depVar :: CodeVarChunk,
  otherVars :: [CodeVarChunk],
  tInit :: CodeExpr,
  tFinal :: CodeExpr,
  initVal :: CodeExpr,
  odeSyst :: [CodeExpr],
  odeOpts :: ODEOptions
}

-- Basic ODEInfo constructor
odeInfo :: CodeVarChunk -> CodeVarChunk -> [CodeVarChunk] -> CodeExpr -> CodeExpr -> 
  CodeExpr -> [CodeExpr] -> ODEOptions -> ODEInfo
odeInfo = ODEInfo

data ODEOptions = ODEOpts {
  solveMethod :: ODEMethod,
  absTol :: CodeExpr,
  relTol :: CodeExpr,
  stepSize :: CodeExpr,
  initValFstOrd :: CodeExpr -- Holds the initial value of a second order ODE.
}

-- Basic odeOptions constructor
odeOptions :: ODEMethod -> CodeExpr -> CodeExpr -> CodeExpr -> CodeExpr -> ODEOptions
odeOptions = ODEOpts

-- Runge-Kutta 4-5, Backwards Differentiation Formula, or Adams' method.
data ODEMethod = RK45 | BDF | Adams
