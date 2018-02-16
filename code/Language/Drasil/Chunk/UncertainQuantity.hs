{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , UncertainChunk(..)
  , uq, uqNU
  , uqc, uqcNU
  , uqcND
  , uncrtnChunk, uvc
  , uncrtnw
  ) where
  
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Constrained
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
  UQ :: (Quantity c, Constrained c, Concept c) => c
        -> Maybe Double -> UncertQ
  
instance Eq UncertQ where
  a == b = (a ^. id) == (b ^. id)
instance Chunk UncertQ where
  id = qlens id
instance NamedIdea UncertQ where
  term = qlens term
instance Idea UncertQ where
  getA (UQ q _) = getA q
instance Quantity UncertQ where
  typ = qlens typ
  getSymb s  (UQ q _) = getSymb s q
  getUnit    (UQ q _) = getUnit q
  getStagedS (UQ q _) = getStagedS q
instance UncertainQuantity UncertQ where
  uncert f (UQ a b) = fmap (\x -> UQ a x) (f b)
instance Constrained UncertQ where
  constraints = qlens constraints
  reasVal = qlens reasVal
instance Definition UncertQ where
  defn = qlens defn
instance ConceptDomain UncertQ where
  cdom = qlens cdom
instance Concept UncertQ where

-- DO NOT Export qlens
qlens :: (forall c. (Quantity c, Constrained c, Concept c) =>
  Simple Lens c a) -> Simple Lens UncertQ a
qlens l f (UQ q u) = fmap (\x -> UQ (set l x q) u) (f (q ^. l))

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (Quantity c, Constrained c, Concept c) =>
  c -> Double -> UncertQ
uq q u = UQ q (Just u)

uqNU :: (Quantity c, Constrained c, Concept c) =>
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

--uncertainty quanity constraint no description
uqcND :: (Unit u) => String -> NP -> Symbol -> u 
                  -> Space -> [Constraint]
                  -> Expr -> Double -> UncertQ
uqcND nam trm sym un space cs val uncrt = uq
  (cuc' nam trm "" sym un space cs val) uncrt

{--}

data UncertainChunk where 
  UCh :: (Quantity c, Constrained c) => c
        -> Maybe Double -> UncertainChunk

instance Eq UncertainChunk where
  (UCh c1 _) == (UCh c2 _) = (c1 ^. id) == (c2 ^. id)
instance Chunk UncertainChunk where
  id = cLens id
instance NamedIdea UncertainChunk where
  term = cLens term
instance Idea UncertainChunk where
  getA (UCh n _) = getA n
instance Quantity UncertainChunk where
  typ = cLens typ
  getSymb s  (UCh c _) = getSymb s c
  getUnit    (UCh c _) = getUnit c
  getStagedS (UCh c _) = getStagedS c
instance UncertainQuantity UncertainChunk where --makes sense?
  uncert f (UCh a b) = fmap (\x -> UCh a x) (f b)
instance Constrained UncertainChunk where
  constraints = cLens constraints
  reasVal = cLens reasVal

-- DO NOT Export cLens
cLens :: (forall c. (Quantity c, Constrained c) =>
  Simple Lens c a) -> Simple Lens UncertainChunk a
cLens l f (UCh q u) = fmap (\x -> UCh (set l x q) u) (f (q ^. l))

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uncrtnChunk :: (Quantity c, Constrained c) =>
  c -> Double -> UncertainChunk
uncrtnChunk q u = UCh q (Just u)

-- | Creates an uncertain varchunk
uvc :: String -> NP -> Symbol 
                -> Space -> [Constraint]
                -> Expr -> Double -> UncertainChunk
uvc nam trm sym space cs val uncrt = uncrtnChunk
  (cvc nam trm sym space cs val) uncrt

uncrtnw :: (UncertainQuantity c, Constrained c) => c -> UncertainChunk
uncrtnw c = UCh c (c ^. uncert)
