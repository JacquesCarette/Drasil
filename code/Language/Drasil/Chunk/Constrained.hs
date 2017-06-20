{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Constrained where

import Control.Lens (Simple, Lens, (^.), set)
import Language.Drasil.Expr (Expr, Relation)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Unit
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol
import Language.Drasil.Chunk

import Prelude hiding (id)

{-
The lists of Constraints below can be implemented in multiple ways.
For ease of writing I think having a list of functions that take
a chunk as an expression would be the easiest to write. 
ie. [:> 7] or [:> 0, :< C someChunk]
Then we would infer the chunk.
This could also be written as [\c -> C c :> 7] which would have a
slightly different type [c -> Relation], but that is less clear.
-}


-- | A Constrained is a 'Quantity' that has value constraints
class Quantity c => Constrained c where
  constraints :: Simple Lens c [Constraint]
  --reasVal :: Simple Lens c Expr
  
data Constraint where
  Phys :: (Expr -> Relation) -> Constraint
  Sfwr :: (Expr -> Relation) -> Constraint
  
physc :: (Expr -> Relation) -> Constraint
physc = Phys

sfwrc :: (Expr -> Relation) -> Constraint
sfwrc = Sfwr 
 
-- | ConstrainedChunks are 'Symbolic Quantities' 
-- with 'Constraints' 
data ConstrainedChunk where
  ConstrainedChunk :: (Quantity c, SymbolForm c) => c 
                        -> [Constraint] -> ConstrainedChunk

instance Chunk ConstrainedChunk where
  id = clens id
instance NamedIdea ConstrainedChunk where
  term = clens term
  getA (ConstrainedChunk n _) = getA n
instance SymbolForm ConstrainedChunk where
  symbol = clens symbol
instance Quantity ConstrainedChunk where
  typ = clens typ
  getSymb (ConstrainedChunk c _) = getSymb c
  getUnit (ConstrainedChunk c _) = getUnit c
instance Constrained ConstrainedChunk where
  constraints f (ConstrainedChunk a b) = 
    fmap (\x -> ConstrainedChunk a x) (f b)
instance Eq ConstrainedChunk where
  (ConstrainedChunk c1 _) == (ConstrainedChunk c2 _) = 
    (c1 ^. id) == (c2 ^. id)

clens :: (forall c. (Quantity c, SymbolForm c) => Simple Lens c a) 
           -> Simple Lens ConstrainedChunk a
clens l f (ConstrainedChunk a b) = 
  fmap (\x -> ConstrainedChunk (set l x a) b) (f (a ^. l))
  
  
-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c, SymbolForm c) => c 
                 -> [Constraint] -> ConstrainedChunk
constrained = ConstrainedChunk
  
-- | Creates a constrained unitary  
cuc :: (Unit u) => String -> NP -> Symbol -> u 
                  -> Space -> [Constraint] -> ConstrainedChunk
cuc i t s u space cs = 
  ConstrainedChunk (unitary i t s u space) cs

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space 
                   -> [Constraint] -> ConstrainedChunk
cvc i des sym space cs = 
  ConstrainedChunk (vc i des sym space) cs