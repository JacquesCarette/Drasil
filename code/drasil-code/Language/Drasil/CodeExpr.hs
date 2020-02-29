module Language.Drasil.CodeExpr (new) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeChunk)

import Control.Lens ((^.))

new :: CodeChunk -> [Expr] -> Expr
new c = New (c ^. uid)