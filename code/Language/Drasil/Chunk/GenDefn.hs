{-# Language GADTs,Rank2Types #-}
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

import Control.Lens (Simple, Lens, (^.), set)

import Prelude hiding (id)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn where
  GD ::  RelationConcept -> Maybe UnitDefn -> Attributes -> GenDefn
  
instance Chunk GenDefn where
  id = rcl id
instance NamedIdea GenDefn where
  term = rcl term
instance Idea GenDefn where
  getA (GD a _ _) = getA a
instance Definition GenDefn where
  defn = rcl defn
instance ConceptDomain GenDefn where
  cdom = rcl cdom
instance Concept GenDefn where
instance ExprRelat GenDefn where
  relat = rcl relat
instance HasAttributes GenDefn where
  attributes f (GD a b c) = fmap (\x -> GD a b x) (f c)

rcl :: Simple Lens RelationConcept a -> Simple Lens GenDefn a
rcl l f (GD a b c) = fmap (\x -> GD (set l x a) b c) (f (a ^. l))

gdUnit :: GenDefn -> Maybe UnitDefn
gdUnit (GD _ u _) = u

gd :: Unit u => RelationConcept -> Maybe u -> Attributes -> GenDefn
gd r (Just u) ats = GD r (Just (unitWrapper u)) ats
gd r Nothing ats = GD r Nothing ats


