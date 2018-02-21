{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC'
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.ExprRelat

import Language.Drasil.NounPhrase (NP)

data RelationConcept where 
  RC :: Concept c => c -> Relation -> RelationConcept

instance Chunk RelationConcept where id = rcl id
instance NamedIdea RelationConcept where term = rcl term
instance Idea RelationConcept where getA (RC c _) = getA c
instance Definition RelationConcept where defn = rcl defn
instance ConceptDomain RelationConcept where cdom = rcl cdom
instance Concept RelationConcept where
instance ExprRelat RelationConcept where relat f (RC c r) = fmap (\x -> RC c x) (f r)

-- don't export this
rcl :: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens RelationConcept a
rcl l f (RC a b) = fmap (\x -> RC (set l x a) b) (f (a ^. l))

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn rel = RC (dccWDS rID rTerm rDefn) rel

-- | Create a RelationConcept from a given id, term, defn, abbreviation, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> RelationConcept
makeRC' rID rTerm rDefn rAbb rel = RC (dccWDS' rID rTerm rDefn rAbb) rel

instance Eq RelationConcept where
  a == b = (a ^. id) == (b ^. id)
