{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Relation (RelationConcept, makeRC, addRelToCC) where

import Control.Lens (makeLenses, (^.), view, set)

import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS, cw)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term),Idea(getA),
  Definition(defn), ConceptDomain(cdom), ExprRelat(relat), Concept)
import Language.Drasil.Expr (Relation)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

data RelationConcept = RC { _conc :: ConceptChunk
                          , rel :: Relation
                          }
makeLenses ''RelationConcept

instance HasUID        RelationConcept where uid = conc . uid
instance NamedIdea     RelationConcept where term = conc . term
instance Idea          RelationConcept where getA = getA . view conc
instance Definition    RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where cdom = cdom . view conc
instance ExprRelat     RelationConcept where relat = rel
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: UID -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn = RC (dccWDS rID rTerm rDefn)

-- | Create a new RelationConcept from an old one, a new id and relation
addRelToCC :: Concept c => c -> UID -> Relation -> RelationConcept
addRelToCC c rID = RC (set uid rID (cw c))
