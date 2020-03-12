module Language.Drasil.Data.ODEInfo (
  ODEInfo(..), odeInfo, ODEOptions(..), odeOptions, ODEMethod(..), 
  ODELibPckg(..), mkODELib
) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.Code.ExternalLibrary (ExternalLibrary)
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang)

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

data ODELibPckg = ODELib {
  libSpec :: ExternalLibrary,
  libCall :: ODEInfo -> ExternalLibraryCall,
  -- It has been said that language information should be in language-related 
  -- file, but this needs more thought. The language would need to declare 
  -- which libraries it is compatible with, but how could it refer to a 
  -- library? Give libraries UID?
  -- Also, this case seems different because ExternalLibraries are data. In 
  -- future, if a user defines a new external library, we wouldn't expect them 
  -- to update an internal Drasil file to add their library to a language's 
  -- compatible libraries. So maybe declaring compatible languages at library 
  -- definition time really is the right way to do this.
  compatibleLangs :: [Lang]
}

mkODELib :: ExternalLibrary -> (ODEInfo -> ExternalLibraryCall) -> [Lang] -> 
  ODELibPckg
mkODELib = ODELib
