{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.GenDefn 
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil.Unit
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Relation

import Control.Lens (makeLenses, view)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD {_rc :: RelationConcept, _mud :: Maybe UnitDefn, _att :: Attributes}
makeLenses ''GenDefn
  
instance Chunk         GenDefn where uid = rc . uid
instance NamedIdea     GenDefn where term = rc . term
instance Idea          GenDefn where getA = getA . view rc
instance Definition    GenDefn where defn = rc . defn
instance ConceptDomain GenDefn where cdom = rc . cdom
instance Concept       GenDefn where
instance ExprRelat     GenDefn where relat = rc . relat
instance HasAttributes GenDefn where attributes = att

gdUnit :: GenDefn -> Maybe UnitDefn
gdUnit = view mud

gd :: IsUnit u => RelationConcept -> Maybe u -> Attributes -> GenDefn
gd r x ats = GD r (fmap unitWrapper x) ats
