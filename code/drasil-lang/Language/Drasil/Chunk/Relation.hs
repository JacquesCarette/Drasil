{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Relation
  ( RelationConcept(..)
  , makeRC, makeRC'
  ) where

import Control.Lens (makeLenses, (^.))

import Language.Drasil.Chunk.Concept (ConceptChunk, cw, dccWDS, dccWDS')
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname))
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term),Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, ExprRelat(relat))
import Language.Drasil.Expr (Relation)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Spec (Sentence)

data RelationConcept = RC { _conc :: ConceptChunk
                          , _rel :: Relation
                          }
makeLenses ''RelationConcept

instance HasUID        RelationConcept where uid = conc . uid
instance NamedIdea     RelationConcept where term = conc . term
instance Idea          RelationConcept where getA (RC c _) = getA c
instance Definition    RelationConcept where defn = conc . defn
instance ConceptDomain RelationConcept where cdom = conc . cdom
instance Concept       RelationConcept where
instance ExprRelat     RelationConcept where relat = rel
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)
instance HasShortName  RelationConcept where
  shortname _ = error "No explicit name given for relation concept -- build a custom Ref"
  --should this be an instance of HasShortName?

-- | Create a RelationConcept from a given id, term, defn, and relation.
makeRC :: String -> NP -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn = RC (dccWDS rID rTerm rDefn)

-- | Create a RelationConcept from a given id, term, defn, abbreviation, and relation.
makeRC' :: String -> NP -> Sentence -> String -> Relation -> RelationConcept
makeRC' rID rTerm rDefn rAbb = RC (cw $ dccWDS' rID rTerm rDefn rAbb)

