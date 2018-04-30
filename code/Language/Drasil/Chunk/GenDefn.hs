{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA))
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.Relation
import Language.Drasil.Unit

import Control.Lens (makeLenses)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn
                  , _attribs :: Attributes
                  }
makeLenses ''GenDefn

instance HasUID GenDefn        where uid = relC . uid
instance NamedIdea GenDefn     where term = relC . term
instance Idea GenDefn          where getA (GD a _ _) = getA a
instance Concept GenDefn
instance Definition GenDefn    where defn = relC . defn
instance ConceptDomain GenDefn where cdom = relC . cdom
instance ExprRelat GenDefn     where relat = relC . relat
instance HasAttributes GenDefn where attributes = attribs

gd :: IsUnit u => RelationConcept -> Maybe u -> Attributes -> GenDefn
gd r (Just u) ats = GD r (Just (unitWrapper u)) ats
gd r Nothing ats = GD r Nothing ats
