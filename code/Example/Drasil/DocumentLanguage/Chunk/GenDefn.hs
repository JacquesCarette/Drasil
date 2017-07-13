{-# Language GADTs,Rank2Types #-}
module Drasil.DocumentLanguage.Chunk.GenDefn 
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil

import Control.Lens (Simple, Lens, (^.), set)

import Prelude hiding (id)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn where
  GD ::  RelationConcept -> Maybe UnitDefn -> GenDefn
  
instance Chunk GenDefn where
  id = rcl id
instance NamedIdea GenDefn where
  term = rcl term
  getA (GD a _) = getA a
instance Concept GenDefn where
  defn = rcl defn
  cdom = rcl cdom
instance ExprRelat GenDefn where
  relat = rcl relat


rcl :: Simple Lens RelationConcept a -> Simple Lens GenDefn a
rcl l f (GD a b) = fmap (\x -> GD (set l x a) b) (f (a ^. l))

gdUnit :: GenDefn -> Maybe UnitDefn
gdUnit (GD _ u) = u

gd :: RelationConcept -> Maybe UnitDefn -> GenDefn
gd = GD
