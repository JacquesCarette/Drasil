{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC'
  ) where

import Control.Lens (makeLenses, (^.))
import Language.Drasil.Expr (Relation)
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term),Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasAttributes(attributes),
  ExprRelat(relat), HasShortName(shortname))
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Attribute.Core (Attributes)

import Language.Drasil.NounPhrase (NP)

data RelationConcept = RC { _conc :: ConceptChunk
                          , _rel :: Relation
                          , _attribs :: Attributes
                          }
makeLenses ''RelationConcept

instance HasUID        RelationConcept where uid = conc . uid
instance NamedIdea     RelationConcept where term = conc . term
instance Idea          RelationConcept where getA (RC c _ _) = getA c
instance Definition    RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where
  type DOM RelationConcept = ConceptChunk
  cdom = conc . cdom
instance Concept       RelationConcept where
instance ExprRelat     RelationConcept where relat = rel
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)
instance HasAttributes RelationConcept where attributes = attribs
instance HasShortName  RelationConcept where

-- | Create a RelationConcept from a given id, term, defn, list of attributes, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> Attributes -> RelationConcept
makeRC rID rTerm rDefn atts = RC (dccWDS rID rTerm rDefn) atts

-- | Create a RelationConcept from a given id, term, defn, abbreviation, list of attributes, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> Attributes -> RelationConcept
makeRC' rID rTerm rDefn rAbb atts = RC (cw $ dccWDS' rID rTerm rDefn rAbb) atts

