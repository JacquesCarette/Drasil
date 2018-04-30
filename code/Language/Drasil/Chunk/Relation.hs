{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC'
  ) where

import Control.Lens (makeLenses, (^.))
import Language.Drasil.Expr (Relation)
import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.ExprRelat

import Language.Drasil.NounPhrase (NP)

data RelationConcept = RC {_conc :: ConceptChunk, _rel :: Relation }
makeLenses ''RelationConcept

instance HasUID        RelationConcept where uid = conc . uid
instance NamedIdea     RelationConcept where term = conc . term
instance Idea          RelationConcept where getA (RC c _) = getA c
instance Definition    RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where cdom = conc . cdom
instance Concept       RelationConcept where
instance ExprRelat     RelationConcept where relat = rel
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn = RC (dccWDS rID rTerm rDefn)

-- | Create a RelationConcept from a given id, term, defn, abbreviation, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> RelationConcept
makeRC' rID rTerm rDefn rAbb = RC (cw $ dccWDS' rID rTerm rDefn rAbb)

