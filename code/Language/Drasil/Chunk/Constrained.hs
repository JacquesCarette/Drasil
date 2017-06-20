{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Constrained (
    Constrained(..), Constraint(..), ConstrainedChunk(..), ConstrConcept(..),
    physc, sfwrc, constrained, cuc, cvc, constrained', cuc'
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Language.Drasil.Expr (Expr, Relation)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Concept
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
  id = qslens id
instance NamedIdea ConstrainedChunk where
  term = qslens term
  getA (ConstrainedChunk n _) = getA n
instance SymbolForm ConstrainedChunk where
  symbol = qslens symbol
instance Quantity ConstrainedChunk where
  typ = qslens typ
  getSymb (ConstrainedChunk c _) = getSymb c
  getUnit (ConstrainedChunk c _) = getUnit c
instance Constrained ConstrainedChunk where
  constraints f (ConstrainedChunk a b) = 
    fmap (\x -> ConstrainedChunk a x) (f b)
instance Eq ConstrainedChunk where
  (ConstrainedChunk c1 _) == (ConstrainedChunk c2 _) = 
    (c1 ^. id) == (c2 ^. id)

qslens :: (forall c. (Quantity c, SymbolForm c) => Simple Lens c a) 
           -> Simple Lens ConstrainedChunk a
qslens l f (ConstrainedChunk a b) = 
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
  
  
  
-- | ConstrConcepts are 'Conceptual Symbolic Quantities' 
-- with 'Constraints' 
data ConstrConcept where
  ConstrConcept :: (Quantity c, SymbolForm c, Concept c) => c 
                        -> [Constraint] -> ConstrConcept

instance Chunk ConstrConcept where
  id = cqslens id
instance NamedIdea ConstrConcept where
  term = cqslens term
  getA (ConstrConcept n _) = getA n
instance SymbolForm ConstrConcept where
  symbol = cqslens symbol
instance Quantity ConstrConcept where
  typ = cqslens typ
  getSymb (ConstrConcept c _) = getSymb c
  getUnit (ConstrConcept c _) = getUnit c
instance Concept ConstrConcept where
  defn = cqslens defn
  cdom = cqslens cdom
instance Constrained ConstrConcept where
  constraints f (ConstrConcept a b) = 
    fmap (\x -> ConstrConcept a x) (f b)
instance Eq ConstrConcept where
  (ConstrConcept c1 _) == (ConstrConcept c2 _) = 
    (c1 ^. id) == (c2 ^. id)
    
    
cqslens :: (forall c. (Quantity c, SymbolForm c, Concept c) => Simple Lens c a) 
           -> Simple Lens ConstrConcept a
cqslens l f (ConstrConcept a b) = 
  fmap (\x -> ConstrConcept (set l x a) b) (f (a ^. l))
  
constrained' :: (Quantity c, SymbolForm c, Concept c) => c 
                 -> [Constraint] -> ConstrConcept
constrained' = ConstrConcept
  
cuc' :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint] -> ConstrConcept
cuc' nam trm desc sym un space cs = 
  ConstrConcept (ucs nam trm desc sym un space) cs