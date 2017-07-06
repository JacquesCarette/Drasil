{-# Language GADTs #-}

module Language.Drasil.Chunk.Theory where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity

import Control.Lens (Simple, Lens)

class Theory t where
  valid_context :: t -> Simple Lens t [t]
  spaces :: t -> Simple Lens t [t] -- FIXME: Should be Simple Lens t [SpaceDefn]
  quantities :: Quantity q => t -> Simple Lens t [q]
  operations :: Concept c => t -> Simple Lens t [c] -- FIXME: Should not be Concept
  defined_quant :: t -> Simple Lens t [QDefinition]
  invariants :: t -> Simple Lens t [Constraint]
  defined_fun :: t -> Simple Lens t [QDefinition]
  
