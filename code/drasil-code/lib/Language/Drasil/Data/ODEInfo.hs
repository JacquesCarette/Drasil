-- | Defines a structure to contain scientifically-relevant information about an ODE.
module Language.Drasil.Data.ODEInfo (
  ODEInfo(..), odeInfo, odeInfo', ODEOptions(..), odeOptions, ODEMethod(..)
) where

import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.CodeExpr.Development
import Language.Drasil(makeAODESolverFormat, formEquations, 
  DifferentialModel(..), ODESolverFormat(..), InitialValueProblem(..))
import Language.Drasil.Chunk.CodeBase (quantvar)

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

-- | Create ODEInfo with Other variables, ODEOptions, DifferentialModel, and InitialValueProblem
odeInfo' :: [CodeVarChunk] -> ODEOptions -> DifferentialModel -> InitialValueProblem -> ODEInfo
odeInfo' ovs opt dm ivp = ODEInfo 
  (quantvar $ _indepVar dm) 
  (quantvar $ _depVar dm) 
  ovs 
  (expr $ initTime ivp)
  (expr $ finalTime ivp)
  (map expr $ initValues ivp)
  (createFinalExpr dm)
  opt

-- | Other parameters for solving the ODE numerically
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

-- | Create well-formatted ODE equations which the ODE solvers can solve.
createFinalExpr :: DifferentialModel -> [CodeExpr]
createFinalExpr dm = map expr $ formEquations (coeffVects ode) (unknownVect ode) (constantVect ode) (_depVar dm)
  where ode = makeAODESolverFormat dm
