-- | Contains functions related to the choice of concept matches.
module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil (UID, Sentence(S), (+:+), (+:+.))

import Language.Drasil.Choices (Choices(..), CodeConcept(..),
    MatchedConceptMap, showChs, Maps(..))

import GOOL.Drasil (SValue, OOProg, MathConstant(..))

import Prelude hiding (pi)
import qualified Data.Map as Map (mapWithKey)
import Control.Monad.State (State, modify)

-- | Concretizes the ConceptMatchMap in Choices to a 'MatchedConceptMap'.
-- Currently we don't have any Choices that would prevent a 'CodeConcept' from
-- being mapped, so we just take the head of the list of 'CodeConcept's
-- The ConceptMatchMap from choices is passed to chooseConcept' internally, this way
-- any 'CodeConcept' list can be matched to its appropiate 'UID'.
chooseConcept :: Choices -> State [Sentence] MatchedConceptMap
chooseConcept chs = sequence $ Map.mapWithKey chooseConcept' (conceptMatch $ maps chs)
  where chooseConcept' :: UID -> [CodeConcept] -> State [Sentence] CodeConcept
        chooseConcept' _ [] = error $ "Empty list of CodeConcepts in the " ++
          "ConceptMatchMap"
        chooseConcept' uid (c:_) = do
            modify (++ [S "Code Concept" +:+ S (show uid) +:+ S "selected as" +:+. showChs c])
            return c

-- | Translates a 'CodeConcept' into GOOL.
conceptToGOOL :: (OOProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi
