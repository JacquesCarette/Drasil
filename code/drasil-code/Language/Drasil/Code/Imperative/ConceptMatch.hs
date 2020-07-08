-- | Contains functions related to the choice of concept matches
module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil (UID)

import Language.Drasil.Choices (Choices(..), CodeConcept(..), 
    MatchedConceptMap, showChs)

import GOOL.Drasil (SValue, OOProg, MathConstant(..))

import Prelude hiding (pi)
import qualified Data.Map as Map (map, mapWithKey)
import Control.Monad.State (State, modify)
import Text.PrettyPrint.HughesPJ (Doc, text, ($$))

-- | Concretizes the ConceptMatchMap in Choices to a MatchedConceptMap.
-- Currently we don't have any Choices that would prevent a CodeConcept from 
-- being mapped, so we just take the head of the list of CodeConcepts
-- The conceptMatchMap from choices is passed to chooseConept' internally, this way
-- any codeconcept list can be matched to its appropiate UID

chooseConcept :: Choices -> State Doc MatchedConceptMap
chooseConcept chs = sequence . addMatchLog $ Map.map (chooseConcept' chs) (conceptMatch chs)
  where chooseConcept' _ [] = error $ "Empty list of CodeConcepts in the " ++ 
          "ConceptMatchMap"
        chooseConcept' _ cs = return $ head cs
        addMatchLog = Map.mapWithKey addMessage
        
addMessage :: UID -> State Doc CodeConcept -> State Doc CodeConcept
addMessage uid s' = do 
    s <- s'
    modify ($$ (text $ "Code Concept "++ uid ++" selected as " ++ showChs s))
    return s

conceptToGOOL :: (OOProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi