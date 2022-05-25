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
-- | Structure to hold ODE information.
data ODEInfo = ODEInfo {
  -- | Independent variable.
  indepVar :: CodeVarChunk,
  -- | Dependent variable.
  depVar :: CodeVarChunk,
  -- | Other variables in the ODE.
  otherVars :: [CodeVarChunk],
  tInit :: CodeExpr,
  tFinal :: CodeExpr,
  -- | Initial value of an ODE.
  initVal :: [CodeExpr],
  -- | ODE equations.
  odeSyst :: [CodeExpr],
  -- | Various options related to the ODE, including solution method, step size, initial value of a second order ODE, etc.
  odeOpts :: ODEOptions
}

-- | Basic 'ODEInfo' constructor.
odeInfo :: CodeVarChunk -> CodeVarChunk -> [CodeVarChunk] -> CodeExpr -> CodeExpr -> 
  [CodeExpr] -> [CodeExpr] -> ODEOptions -> ODEInfo
odeInfo = ODEInfo

data ODEOptions = ODEOpts {
  -- | Solution method.
  solveMethod :: ODEMethod,
  -- | Absolute tolerance.
  absTol :: CodeExpr,
  -- | Relative tolerance.
  relTol :: CodeExpr,
  -- | Step size.
  stepSize :: CodeExpr
}

-- | Basic 'ODEOptions' constructor
odeOptions :: ODEMethod -> CodeExpr -> CodeExpr -> CodeExpr -> ODEOptions
odeOptions = ODEOpts

-- | Methods for solving ODEs. Includes Runge-Kutta 4-5, Backwards Differentiation Formula, or Adams' method.
data ODEMethod = RK45 | BDF | Adams
