module Language.Drasil.CodeExpr (new) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeChunk)

new :: CodeChunk -> [Expr] -> Expr
new c = New (sy c)