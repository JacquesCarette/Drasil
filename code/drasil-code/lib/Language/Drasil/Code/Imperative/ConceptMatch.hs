-- | Contains functions related to the choice of concept matches.
module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL, typeDefaultValue
) where

import Prelude hiding (pi)
import qualified Data.Map as Map (mapWithKey)
import Control.Monad.State (State, modify)

import Drasil.Database (UID)
import Language.Drasil (Sentence(S), (+:+), (+:+.))
import Drasil.GOOL (SValue, MathConstant(..), OOTypeSym, CodeType(..),
  Literal(..), convTypeOO)

import Language.Drasil.Choices (Choices(..), CodeConcept(..),
    MatchedConceptMap, showChs, Maps(..))

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
conceptToGOOL :: (MathConstant r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi

-- | Gives a default value for a given type
typeDefaultValue :: (Literal r, OOTypeSym r) => CodeType -> SValue r
typeDefaultValue Boolean = litFalse
typeDefaultValue Integer = litInt 0
typeDefaultValue Float = litFloat 0.0
typeDefaultValue Double = litDouble 0.0
typeDefaultValue Char = litChar ' '
typeDefaultValue String = litString ""
typeDefaultValue (List t) = litList (convTypeOO t) []
typeDefaultValue (Array t) = litArray (convTypeOO t) []
typeDefaultValue (Set t) = litSet (convTypeOO t) []
typeDefaultValue (Reference t) = typeDefaultValue t
typeDefaultValue t = error $ "Attempt to get default value for type with none: " ++ show t
