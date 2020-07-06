-- | Contains functions related to the choice of concept matches
module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil (UID)

import Language.Drasil.Choices (Choices(..), CodeConcept(..), ConceptMatchMap, 
    MatchedConceptMap, showChs)

import GOOL.Drasil (SValue, OOProg, MathConstant(..))

import Prelude hiding (pi)
import qualified Data.Map as Map (map, keys, lookup)
import Control.Monad.State (State, modify)
import Text.PrettyPrint.HughesPJ (Doc, text, ($$))

-- | Concretizes the ConceptMatchMap in Choices to a MatchedConceptMap.
-- Currently we don't have any Choices that would prevent a CodeConcept from 
-- being mapped, so we just take the head of the list of CodeConcepts
-- The conceptMatchMap from choices is passed to chooseConept' internally, this way
-- any codeconcept list can be matched to its appropiate UID
chooseConcept :: Choices -> State Doc MatchedConceptMap
chooseConcept chs = sequence $ Map.map (chooseConcept' chs $ conceptMatch chs) (conceptMatch chs)
  where chooseConcept' :: Choices -> ConceptMatchMap-> [CodeConcept] -> State Doc CodeConcept
        chooseConcept' _ _ [] = error $ "Empty list of CodeConcepts in the " ++ 
          "ConceptMatchMap"
        chooseConcept' _ cmm cs = do 
            let uid = findUid (Map.keys cmm) cmm cs
            modify ($$ (text $ "Code Concept "++ uid ++" selected as " ++ showChs (head cs)))
            return $ head cs

-- | Maps CodeConcepts to corresponding GOOL values
conceptToGOOL :: (OOProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi

findUid :: [UID] -> ConceptMatchMap -> [CodeConcept] -> UID 
findUid [] _ _ = error "CodeConcept not in Concept Match Map"
findUid (x:xs) mapp cs = if Map.lookup x mapp == (Just cs) then x else findUid xs mapp cs 