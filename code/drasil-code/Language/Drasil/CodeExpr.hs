module Language.Drasil.CodeExpr (new) where

import Language.Drasil

import Language.Drasil.Chunk.Code (CodeChunk(..), VarOrFunc(..))

import Control.Lens ((^.))

new :: CodeChunk -> [Expr] -> Expr
new c ps = checkFunc (kind c)
  where checkFunc Var = error $ "Attempt to create new actor but passed " ++
          "CodeChunk is not callable"
        checkFunc Func = New (c ^. uid) ps