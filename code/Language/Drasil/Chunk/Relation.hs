{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC'
  ) where

import Control.Lens (makeLenses, (^.))
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.ExprRelat

import Language.Drasil.NounPhrase (NP)

data RelationConcept = RC {_conc :: ConceptChunk, _rel :: Relation }
makeLenses ''RelationConcept

instance Chunk RelationConcept where id = conc . id
instance NamedIdea RelationConcept where term = conc . term
instance Idea RelationConcept where getA (RC c _) = getA c
instance Definition RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where cdom = conc . cdom
instance Concept RelationConcept where
instance ExprRelat RelationConcept where relat f (RC c r) = fmap (\x -> RC c x) (f r)
instance Eq RelationConcept where a == b = (a ^. id) == (b ^. id)

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn rel = RC (dccWDS rID rTerm rDefn) rel

-- | Create a RelationConcept from a given id, term, defn, abbreviation, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> RelationConcept
makeRC' rID rTerm rDefn rAbb rel = RC (dccWDS' rID rTerm rDefn rAbb) rel

