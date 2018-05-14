module Language.Drasil.Chunk.ExprRelat (ExprRelat (relat)) where

import Language.Drasil.Expr (Expr)

import Control.Lens (Lens')

class ExprRelat c where
  relat :: Lens' c Expr
