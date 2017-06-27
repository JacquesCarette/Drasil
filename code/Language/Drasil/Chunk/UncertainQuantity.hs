{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , uq
  ) where
  
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Control.Lens ((^.), set, Simple, Lens)

import Prelude hiding (id)

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Simple Lens c Double
  
-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- Quantity and a Double (from 0 to 1) which represents a percentage of uncertainty
data UncertQ where
  UQ :: Quantity c => c -> Double -> UncertQ
  
instance Eq UncertQ where
  a == b = (a ^. id) == (b ^. id)
instance Chunk UncertQ where
  id = qlens id
instance NamedIdea UncertQ where
  term = qlens term
  getA (UQ q _) = getA q
instance Quantity UncertQ where
  typ = qlens typ
  getSymb (UQ q _) = getSymb q
  getUnit (UQ q _) = getUnit q
instance UncertainQuantity UncertQ where
  uncert f (UQ a b) = fmap (\x -> UQ a x) (f b)
  
-- DO NOT Export qlens
qlens :: (forall c. (Quantity c) => Simple Lens c a) -> Simple Lens UncertQ a
qlens l f (UQ q u) = fmap (\x -> UQ (set l x q) u) (f (q ^. l))

-- | The UncertainQuantity constructor. Requires a Quantity and a percentage
uq :: Quantity c => c -> Double -> UncertQ
uq = UQ
