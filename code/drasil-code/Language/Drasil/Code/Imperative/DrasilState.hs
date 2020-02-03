module Language.Drasil.Code.Imperative.DrasilState (
  DrasilState(..)
) where

import Language.Drasil
import Language.Drasil.CodeSpec (AuxFile, CodeSpec, Modularity, Comments, 
  Verbosity, MatchedConceptMap, ConstantRepr, ConstantStructure, 
  ConstraintBehaviour, InputModule, Logging, Structure)

-- Private State, used to push these options around the generator
data DrasilState = DrasilState {
  codeSpec :: CodeSpec,
  date :: String,
  modular :: Modularity,
  inStruct :: Structure,
  conStruct :: ConstantStructure,
  conRepr :: ConstantRepr,
  inMod :: InputModule,
  logName :: String,
  logKind :: Logging,
  commented :: [Comments],
  doxOutput :: Verbosity,
  concMatches :: MatchedConceptMap,
  auxiliaries :: [AuxFile],
  sampleData :: [Expr],
  currentModule :: String,

  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour
}