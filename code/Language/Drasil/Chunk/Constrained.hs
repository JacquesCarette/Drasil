module Language.Drasil.Chunk.Constrained where

import Control.Lens (Simple, Lens)
import Language.Drasil.Expr (Expr, Relation)
{-
The lists of Constraints below can be implemented in multiple ways.
For ease of writing I think having a list of functions that take
a chunk as an expression would be the easiest to write. 
ie. [:> 7] or [:> 0, :< C someChunk]
Then we would infer the chunk.
This could also be written as [\c -> C c :> 7] which would have a
slightly different type [c -> Relation], but that is less clear.
-}

--CURRENTLY NOT REQUIRED TO BE AN INSTANCE OF ANYTHING, THAT SHOULD
-- (MOST LIKELY) CHANGE.

class Constrained c where
  physConstraints :: Simple Lens c [Expr -> Relation]
  sfwrConstraints :: Simple Lens c [Expr -> Relation]
  reasVal :: Simple Lens c Expr
