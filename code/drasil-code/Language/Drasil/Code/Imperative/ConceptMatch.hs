module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil.CodeSpec (Choices(..), CodeConcept(..), 
  MatchedConceptMap)

import GOOL.Drasil (ProgramSym, ValueSym(..), GS)

import Prelude hiding (pi)
import qualified Data.Map as Map (map)

-- Currently we don't have any Choices that would prevent a CodeConcept from being mapped, so we just take the head of the list of CodeConcepts
chooseConcept :: Choices -> MatchedConceptMap
chooseConcept chs = Map.map (chooseConcept' chs) (conceptMatch chs)
  where chooseConcept' _ [] = error $ "Empty list of CodeConcepts in the " ++ 
          "ConceptMatchMap"
        chooseConcept' _ cs = head cs

conceptToGOOL :: (ProgramSym repr) => CodeConcept -> GS (repr (Value repr))
conceptToGOOL Pi = pi

