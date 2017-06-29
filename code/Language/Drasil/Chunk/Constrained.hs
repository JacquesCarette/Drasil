{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Constrained (
    Constrained(..)
  , Constraint(..)
  , ConstrainedChunk(..)
  , ConstrConcept(..)
  , physc, sfwrc, constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , ConstrWrapper(..), cnstrw
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Language.Drasil.Expr (Expr(..), Relation)
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
-- and maybe reasonable value
class Quantity c => Constrained c where
  constraints :: Simple Lens c [Constraint]
  reasVal     :: Simple Lens c (Maybe Expr)

data Constraint where
  Phys :: (Expr -> Relation) -> Constraint
  Sfwr :: (Expr -> Relation) -> Constraint
  
physc :: (Expr -> Relation) -> Constraint
physc = Phys

sfwrc :: (Expr -> Relation) -> Constraint
sfwrc = Sfwr 
 
-- | ConstrainedChunks are 'Symbolic Quantities' 
-- with 'Constraints' and maybe typical value
data ConstrainedChunk where
  ConstrainedChunk :: (Quantity c, SymbolForm c) => c 
                        -> [Constraint] -> Maybe Expr -> ConstrainedChunk

instance Chunk ConstrainedChunk where
  id = qslens id
instance NamedIdea ConstrainedChunk where
  term = qslens term
  getA (ConstrainedChunk n _ _) = getA n
instance SymbolForm ConstrainedChunk where
  symbol = qslens symbol
instance Quantity ConstrainedChunk where
  typ = qslens typ
  getSymb (ConstrainedChunk c _ _) = getSymb c
  getUnit (ConstrainedChunk c _ _) = getUnit c
instance Constrained ConstrainedChunk where
  constraints f (ConstrainedChunk a b c) = 
    fmap (\x -> ConstrainedChunk a x c) (f b)
  reasVal f (ConstrainedChunk a b c) = 
    fmap (\x -> ConstrainedChunk a b x) (f c)
instance Eq ConstrainedChunk where
  (ConstrainedChunk c1 _ _) == (ConstrainedChunk c2 _ _) = 
    (c1 ^. id) == (c2 ^. id)

qslens :: (forall c. (Quantity c, SymbolForm c) => Simple Lens c a) 
           -> Simple Lens ConstrainedChunk a
qslens l f (ConstrainedChunk a b c) = 
  fmap (\x -> ConstrainedChunk (set l x a) b c) (f (a ^. l))
  
  
-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c, SymbolForm c) => c 
                -> [Constraint] -> Expr -> ConstrainedChunk
constrained q cs ex = ConstrainedChunk q cs (Just ex)
  
-- | Creates a constrained unitary  
cuc :: (Unit u) => String -> NP -> Symbol -> u 
                -> Space -> [Constraint] -> Maybe Expr -> ConstrainedChunk
cuc i t s u space cs rv = 
  ConstrainedChunk (unitary i t s u space) cs rv

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space 
       -> [Constraint] -> Maybe Expr -> ConstrainedChunk
cvc i des sym space cs rv = 
  ConstrainedChunk (vc i des sym space) cs rv
  
  
  
-- | ConstrConcepts are 'Conceptual Symbolic Quantities' 
-- with 'Constraints' and maybe a reasonable value
data ConstrConcept where
  ConstrConcept :: (Quantity c, SymbolForm c, Concept c) => c 
                        -> [Constraint] -> Maybe Expr -> ConstrConcept

instance Chunk ConstrConcept where
  id = cqslens id
instance NamedIdea ConstrConcept where
  term = cqslens term
  getA (ConstrConcept n _ _) = getA n
instance SymbolForm ConstrConcept where
  symbol = cqslens symbol
instance Quantity ConstrConcept where
  typ = cqslens typ
  getSymb (ConstrConcept c _ _) = getSymb c
  getUnit (ConstrConcept c _ _) = getUnit c
instance Concept ConstrConcept where
  defn = cqslens defn
  cdom = cqslens cdom
instance Constrained ConstrConcept where
  constraints f (ConstrConcept a b c) = 
    fmap (\x -> ConstrConcept a x c) (f b)
  reasVal f (ConstrConcept a b c) = 
    fmap (\x -> ConstrConcept a b x) (f c)
instance Eq ConstrConcept where
  (ConstrConcept c1 _ _) == (ConstrConcept c2 _ _) = 
    (c1 ^. id) == (c2 ^. id)

cqslens :: (forall c. (Quantity c, SymbolForm c, Concept c) => Simple Lens c a)
           -> Simple Lens ConstrConcept a
cqslens l f (ConstrConcept a b c) = 
  fmap (\x -> ConstrConcept (set l x a) b c) (f (a ^. l))
  
constrained' :: (Quantity c, SymbolForm c, Concept c) => c 
                 -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept q cs (Just rv)

constrainedNRV' :: (Quantity c, SymbolForm c, Concept c) => c 
                 -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept q cs Nothing
  
cuc' :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv = 
  ConstrConcept (ucs nam trm desc sym un space) cs (Just rv)

-- ConstraintWrapper for wrapping anything that is constrained
data ConstrWrapper where
  CnstrW :: (Constrained c, SymbolForm c) => c -> ConstrWrapper

instance Chunk ConstrWrapper where
  id = cwlens id
instance Eq ConstrWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Constrained ConstrWrapper where
  constraints = cwlens constraints
  reasVal = cwlens reasVal
instance NamedIdea ConstrWrapper where
  term = cwlens term
  getA (CnstrW a) = getA a
instance SymbolForm ConstrWrapper where
  symbol = cwlens symbol
instance Quantity ConstrWrapper where
  getSymb (CnstrW a) = getSymb a
  getUnit (CnstrW a) = getUnit a
  typ = cwlens typ

cnstrw :: (Constrained c, SymbolForm c) => c -> ConstrWrapper
cnstrw = CnstrW

-- do not export
cwlens :: (forall c. (Constrained c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens ConstrWrapper a
cwlens l f (CnstrW a) = fmap (\x -> CnstrW (set l x a)) (f (a ^. l))