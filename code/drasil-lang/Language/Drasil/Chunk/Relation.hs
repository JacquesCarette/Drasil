{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC', conc
  ) where

import Control.Lens (makeLenses, (^.))

import Language.Drasil.Chunk.Concept (ConceptChunk, cw, dccWDS, dccWDS')
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term),Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, ExprRelat(relat),
  HasLabel(getLabel), HasShortName(shortname))
import Language.Drasil.Expr (Relation)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)

data RelationConcept = RC { _conc :: ConceptChunk
                          , _rel :: Relation
                          , _lbl :: Label
                          }
makeLenses ''RelationConcept

instance HasUID        RelationConcept where uid = conc . uid
instance NamedIdea     RelationConcept where term = conc . term
instance Idea          RelationConcept where getA (RC c _ _) = getA c
instance Definition    RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where cdom = conc . cdom
instance Concept       RelationConcept where
instance ExprRelat     RelationConcept where relat = rel
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)
instance HasShortName  RelationConcept where
  shortname _ = error "No explicit name given for relation concept -- build a custom Ref"
  --should this be an instance of HasShortName?
instance HasLabel      RelationConcept where getLabel = lbl

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> Label -> RelationConcept
makeRC rID rTerm rDefn label = RC (dccWDS rID rTerm rDefn) label

-- | Create a RelationConcept from a given id, term, defn, abbreviation, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> Label -> RelationConcept
makeRC' rID rTerm rDefn rAbb label = RC (cw $ dccWDS' rID rTerm rDefn rAbb) label

