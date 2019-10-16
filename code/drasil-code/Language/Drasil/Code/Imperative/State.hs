module Language.Drasil.Code.Imperative.State (
  State(..)
) where

import Language.Drasil
import Language.Drasil.CodeSpec (AuxFile, CodeSpec, Comments, ConceptMatchMap, 
  ConstantRepr, ConstantStructure, ConstraintBehaviour, InputModule, Logging, 
  Structure)

-- Private State, used to push these options around the generator
data State = State {
  codeSpec :: CodeSpec,
  date :: String,
  inStruct :: Structure,
  conStruct :: ConstantStructure,
  conRepr :: ConstantRepr,
  inMod :: InputModule,
  logName :: String,
  logKind :: Logging,
  commented :: [Comments],
  concMatches :: ConceptMatchMap,
  auxiliaries :: [AuxFile],
  sampleData :: [Expr],
  currentModule :: String,

  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour
}