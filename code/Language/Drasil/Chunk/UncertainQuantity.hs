{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , uq
  , typVal
  , uqc
  , cCnptfromUQ
  ) where
  
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Concept
import Language.Drasil.Unit
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol
import Control.Lens ((^.), set, Simple, Lens)

import Prelude hiding (id)

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Simple Lens c Double

{-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- Quantity and a Double (from 0 to 1) which represents a percentage of uncertainty-}
-- UQ takes a constrained chunk, an uncertainty (between 0 and 1), and a typical value
--FIXME: typical value should be able to handle constrained chunks that are constrainted in Integers
--       so they should have an Integer typical value

data UncertQ where
  UQ :: ConstrConcept -> Double -> Double -> UncertQ
  
instance Eq UncertQ where
  a == b = (a ^. id) == (b ^. id)
instance Chunk UncertQ where
  id = qlens id
instance NamedIdea UncertQ where
  term = qlens term
  getA (UQ q _ _) = getA q
instance Quantity UncertQ where
  typ = qlens typ
  getSymb (UQ q _ _) = getSymb q
  getUnit (UQ q _ _) = getUnit q
instance UncertainQuantity UncertQ where
  uncert f (UQ a b c) = fmap (\x -> UQ a x c) (f b)
instance Constrained UncertQ where
  constraints = qlens constraints
instance SymbolForm UncertQ where
  symbol = qlens symbol
instance Concept UncertQ where
  defn = qlens defn
  cdom = qlens cdom

typVal :: UncertQ -> Double
typVal (UQ _ _ v) = v

cCnptfromUQ :: UncertQ -> ConstrConcept
cCnptfromUQ (UQ c _ _) = c

uqc :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint]
                  -> Double -> Double -> UncertQ
uqc nam trm desc sym un space cs uncrt val = uq
  (cuc' nam trm desc sym un space cs) uncrt val

-- DO NOT Export qlens
qlens :: (Simple Lens ConstrConcept a) -> Simple Lens UncertQ a
qlens l f (UQ q u v) = fmap (\x -> UQ (set l x q) u v) (f (q ^. l))

-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: ConstrConcept -> Double -> Double -> UncertQ
uq = UQ
