module Language.Drasil.Chunk.ExprRelat where

import Language.Drasil.Chunk
import Language.Drasil.Expr

import Control.Lens (Simple, Lens)

class Chunk c => ExprRelat c where
  relat :: Simple Lens c Expr
