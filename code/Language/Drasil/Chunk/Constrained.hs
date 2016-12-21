module Language.Drasil.Chunk.Constrained(Constrained(..), 
    ConstrainedMUC(..),fromMUC) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Expr (Expr, Relation)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.MUChunk


class Constrained c where
  physConstraints :: Simple Lens c [Expr -> Relation]
  sfwrConstraints :: Simple Lens c [Expr -> Relation]
  reasVal :: Simple Lens c Expr

--Currently testing with Constrained MUC only, could probably change
--this to "ConstrainedChunk" and have constructors for CMUC, CEq, etc.
data ConstrainedMUC = CMUC 
  { muc :: MUChunk, 
    pc :: [Expr -> Relation], -- physical constraints 
    -- (Expects a Chunk, to be filled in later), for more natural writing.
    -- ex. the chunk x is constrained to be greater than 7, so we should
    --    write (within the constraints) [:> 7] and "x" is inferred
    sc :: [Expr -> Relation], --software constraints
    rv :: Expr  -- reasonable value (number?)
  }
  
instance Chunk ConstrainedMUC where
  name = ul . name

instance NamedIdea ConstrainedMUC where
  term = ul . term
  
instance SymbolForm ConstrainedMUC where
  symbol = ul . symbol
  
--[S/Q]
-- instance Unit' ConstrainedMUC where
--  unit' = ul . unit'
  
instance Constrained ConstrainedMUC where
  physConstraints f (CMUC a b c d) = fmap (\x -> CMUC a x c d) (f b)
  sfwrConstraints f (CMUC a b c d) = fmap (\x -> CMUC a b x d) (f c)
  reasVal         f (CMUC a b c d) = fmap (\x -> CMUC a b c x) (f d)

ul :: Simple Lens ConstrainedMUC MUChunk
ul f (CMUC a b c d) = fmap (\x -> CMUC x b c d) (f a)

fromMUC :: MUChunk -> [Expr->Relation] -> [Expr->Relation] -> Expr -> ConstrainedMUC
fromMUC muChunk pcs scs r = CMUC muChunk pcs scs r