module Language.Drasil.Chunk.Constrained where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Expr (Expr, Relation)
import Language.Drasil.Chunk
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

 
{- DEPRECATED CODE BELOW

--Currently testing with Constrained MUC only, could probably change
--this to "ConstrainedChunk" and have constructors for CMUC, CEq, etc.
data ConstrainedMUC = CMUC 
  { muc :: MUChunk, 
    pc :: [Expr -> Relation], -- physical constraints 
    sc :: [Expr -> Relation], --software constraints
    rv :: Expr  -- reasonable value (number?)
  }
  
instance Chunk ConstrainedMUC where
  id = ul . id

instance NamedIdea ConstrainedMUC where
  term = ul . term
  
instance SymbolForm ConstrainedMUC where
  symbol = ul . symbol
  

  
instance Constrained ConstrainedMUC where
  physConstraints f (CMUC a b c d) = fmap (\x -> CMUC a x c d) (f b)
  sfwrConstraints f (CMUC a b c d) = fmap (\x -> CMUC a b x d) (f c)
  reasVal         f (CMUC a b c d) = fmap (\x -> CMUC a b c x) (f d)

ul :: Simple Lens ConstrainedMUC MUChunk
ul f (CMUC a b c d) = fmap (\x -> CMUC x b c d) (f a)
-}