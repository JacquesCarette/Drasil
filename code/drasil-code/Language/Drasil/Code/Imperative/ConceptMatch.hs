-- | Contains functions related to the choice of concept matches
module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil.Choices (Choices(..), CodeConcept(..), ConceptMatchMap, 
    MatchedConceptMap, showChs)

import GOOL.Drasil (SValue, OOProg, MathConstant(..))

import Prelude hiding (pi)
import qualified Data.Map as Map (map, keys)
import Control.Monad.State (State, modify)
import Text.PrettyPrint.HughesPJ (Doc, text, ($$))

-- | Concretizes the ConceptMatchMap in Choices to a MatchedConceptMap.
-- Currently we don't have any Choices that would prevent a CodeConcept from 
-- being mapped, so we just take the head of the list of CodeConcepts
chooseConcept :: Choices -> State Doc MatchedConceptMap
chooseConcept chs = sequence $ Map.map (chooseConcept' chs $ conceptMatch chs) (conceptMatch chs)
  where chooseConcept' :: Choices -> ConceptMatchMap-> [CodeConcept] -> State Doc CodeConcept
        chooseConcept' _ _ [] = error $ "Empty list of CodeConcepts in the " ++ 
          "ConceptMatchMap"
        chooseConcept' _ cmm cs = do 
            let uid = (head . Map.keys) cmm
            modify ($$ (text $ "Code Concept "++ uid ++" selected as " ++ showChs (head cs)))
            return $ head cs

-- | Maps CodeConcepts to corresponding GOOL values
conceptToGOOL :: (OOProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi

