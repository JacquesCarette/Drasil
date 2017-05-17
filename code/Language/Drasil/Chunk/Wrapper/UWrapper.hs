{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper.UWrapper
  ( uw
  , UWrapper
  , ucw
  , UCWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Unitary
import Prelude hiding (id)

-- | Unitary Wrapper
data UWrapper where
  UW :: (Unitary c, SymbolForm c) => c -> UWrapper
  
ulens :: (forall c. (Unitary c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens UWrapper a
ulens l f (UW a) = fmap (\x -> UW (set l x a)) (f (a ^. l))

instance Eq UWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UWrapper where
  compare a b = compare (a ^. symbol) (b ^. symbol)

instance Chunk UWrapper where
  id = ulens id
instance NamedIdea UWrapper where
  term = ulens term
  getA (UW a) = getA a
instance Quantity UWrapper where
  typ = ulens typ
  getSymb (UW a) = getSymb a
  getUnit (UW a) = getUnit a
instance Unitary UWrapper where
  unit (UW a) = unit a
instance SymbolForm UWrapper where
  symbol = ulens symbol
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
uw :: (Unitary c, SymbolForm c) => c -> UWrapper
uw = UW

-- | Unitary __and__ Concept Wrapper
data UCWrapper where
  UCW :: (Unitary c, SymbolForm c, Concept c) => c -> UCWrapper
  
uclens :: (forall c. (Unitary c, SymbolForm c, Concept c) => 
  Simple Lens c a) -> Simple Lens UCWrapper a
uclens l f (UCW a) = fmap (\x -> UCW (set l x a)) (f (a ^. l))

instance Eq UCWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UCWrapper where
  compare a b = compare (a ^. symbol) (b ^. symbol)

instance Chunk UCWrapper where
  id = uclens id
instance NamedIdea UCWrapper where
  term = uclens term
  getA (UCW a) = getA a
instance Quantity UCWrapper where
  typ = uclens typ
  getSymb (UCW a) = getSymb a
  getUnit (UCW a) = getUnit a
instance Unitary UCWrapper where
  unit (UCW a) = unit a
instance SymbolForm UCWrapper where
  symbol = uclens symbol
instance Concept UCWrapper where
  defn = uclens defn
  cdom = uclens cdom
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
ucw :: (Unitary c, SymbolForm c, Concept c) => c -> UCWrapper
ucw = UCW

