module Language.Drasil.Code.Imperative.DrasilState (
  DrasilState(..), inMod, ModExportMap, ClassDefinitionMap
) where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeVarChunk)
import Language.Drasil.Code.ExtLibImport (ExtLibState)
import Language.Drasil.CodeSpec (AuxFile, CodeSpec, Modularity(..), Comments, 
  Verbosity, MatchedConceptMap, MatchedSpaces, ConstantRepr, ConstantStructure, 
  ConstraintBehaviour, InputModule(..), Logging, Structure)
import Language.Drasil.Mod (Mod)

import Data.Map (Map)

-- Map from calculation function name to the ExtLibState containing the contents of the function
type ExtLibMap = Map String ExtLibState

type VarMap      = Map String CodeVarChunk

-- name of variable/function maps to module name
type ModExportMap = Map String String

-- name of variable/function maps to class name
type ClassDefinitionMap = Map String String

-- Private State, used to push these options around the generator
data DrasilState = DrasilState {
  codeSpec :: CodeSpec,
  date :: String,
  modular :: Modularity,
  inStruct :: Structure,
  conStruct :: ConstantStructure,
  conRepr :: ConstantRepr,
  logName :: String,
  logKind :: Logging,
  commented :: [Comments],
  doxOutput :: Verbosity,
  concMatches :: MatchedConceptMap,
  spaceMatches :: MatchedSpaces,
  auxiliaries :: [AuxFile],
  sampleData :: [Expr],
  modules :: [Mod],
  extLibMap :: ExtLibMap,
  vMap :: VarMap,
  eMap :: ModExportMap,
  cMap :: ClassDefinitionMap,
  defList :: [Name],
  currentModule :: String,
  currentClass :: String,

  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour
}

inMod :: DrasilState -> InputModule
inMod ds = inMod' $ modular ds
  where inMod' Unmodular = Combined
        inMod' (Modular im) = im