{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , uq, uqNU
  , uqc, uqcNU
  , uqcND
  ) where
  
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Concept
import Language.Drasil.Unit
import Language.Drasil.Expr
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol
import Control.Lens ((^.), set, Simple, Lens)

import Prelude hiding (id)

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Simple Lens c (Maybe Double)

{-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- Quantity and a Double (from 0 to 1) which represents a percentage of uncertainty-}
-- UQ takes a constrained chunk, an uncertainty (between 0 and 1), and a typical value

data UncertQ where
  UQ :: (Quantity c, Constrained c, Concept c, SymbolForm c) => c
        -> Maybe Double -> UncertQ
  
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
instance Constrained UncertQ where
  constraints = qlens constraints
  reasVal = qlens reasVal
instance SymbolForm UncertQ where
  symbol = qlens symbol
instance Concept UncertQ where
  defn = qlens defn
  cdom = qlens cdom

-- DO NOT Export qlens
qlens :: (forall c. (Quantity c, Constrained c, Concept c, SymbolForm c) =>
  Simple Lens c a) -> Simple Lens UncertQ a
qlens l f (UQ q u) = fmap (\x -> UQ (set l x q) u) (f (q ^. l))

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (Quantity c, Constrained c, Concept c, SymbolForm c) =>
  c -> Double -> UncertQ
uq q u = UQ q (Just u)

uqNU :: (Quantity c, Constrained c, Concept c, SymbolForm c) =>
  c -> UncertQ
uqNU q = UQ q Nothing

uqc :: (Unit u) => String -> NP -> String -> Symbol -> u 
                -> Space -> [Constraint]
                -> Expr -> Double -> UncertQ
uqc nam trm desc sym un space cs val uncrt = uq
  (cuc' nam trm desc sym un space cs val) uncrt

--uncertainty quanity constraint no uncertainty
uqcNU :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint]
                  -> Expr -> UncertQ
uqcNU nam trm desc sym un space cs val = uqNU $
  cuc' nam trm desc sym un space cs val

--uncertainty quanity constraint no discription
uqcND :: (Unit u) => String -> NP -> Symbol -> u 
                  -> Space -> [Constraint]
                  -> Expr -> Double -> UncertQ
uqcND nam trm sym un space cs val uncrt = uq
  (cuc' nam trm "" sym un space cs val) uncrt