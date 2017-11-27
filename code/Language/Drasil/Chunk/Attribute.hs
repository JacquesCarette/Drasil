{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Attribute where

import Control.Lens (Simple, Lens, (^.), set)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk
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


