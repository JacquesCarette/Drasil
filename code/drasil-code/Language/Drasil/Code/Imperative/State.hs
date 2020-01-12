module Language.Drasil.Code.Imperative.State (
  DrasilState(..)
) where

import Language.Drasil
import Language.Drasil.CodeSpec (AuxFile, CodeSpec, Comments, Verbosity, 
  MatchedConceptMap, ConstantRepr, ConstantStructure, ConstraintBehaviour, 
  InputModule, Logging, Structure)

-- Private State, used to push these options around the generator
data DrasilState = DrasilState {
  codeSpec :: CodeSpec,
  date :: String,
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