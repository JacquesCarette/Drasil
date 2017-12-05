{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Attribute 
  ( Attribute(..), Attributes, HasAttributes(..)
  , AttribQDef(..)
  ) where

import Control.Lens (Simple, Lens, (^.))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.NamedIdea (NamedIdea, term, getA)
import Language.Drasil.Chunk.Quantity (Quantity, typ, getStagedS, getSymb, getUnit)
import Language.Drasil.Document (Contents)


import Prelude hiding (id)

-- | Attributes are just a list of 'Attribute'
type Attributes = [Attribute]

-- | An attribute can be a rationale, a reference to the source (we used) to find
-- this knowledge, or a derivation to show how we arrived 
-- at a given model/definition/etc.
data Attribute = Rationale Sentence
               | SourceRef Sentence -- Source to reference for this knowledge chunk
               | Derivation Contents -- Makes sense for now (derivations are just document sections at the moment), but we may need to create a new
               -- representation for it in the future.

-- | Any chunk with 'Attributes' is part of the 'HasAttributes' class.
class Chunk c => HasAttributes c where
  attributes :: Simple Lens c Attributes

-- | A QDefinition (Quantity) with attributes is an 'AttribQDef'
-- This is a temporary data structure to be used until attributes are fully
-- functional, then it will be fused with QDefinition
data AttribQDef where
  AQD :: QDefinition -> Attributes -> AttribQDef
  
instance Chunk AttribQDef where
  id = qdl . id
instance NamedIdea AttribQDef where
  term = qdl . term
  getA (AQD q _) = getA q
instance Quantity AttribQDef where
  typ = qdl . typ
  getSymb s (AQD q _) = getSymb s q
  getStagedS (AQD q _) = getStagedS q
  getUnit (AQD q _) = getUnit q
instance Eq AttribQDef where
  a == b = (a ^. id) == (b ^. id)

instance HasAttributes AttribQDef where
  attributes f (AQD a b) = fmap (\x -> AQD a x) (f b)
  
-- DO NOT EXPORT
qdl :: Simple Lens AttribQDef QDefinition
qdl f (AQD a b) = fmap (\x -> AQD x b) (f a)


