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
  UW :: (Unitary c) => c -> UWrapper
  
ulens :: (forall c. (Unitary c) => 
  Simple Lens c a) -> Simple Lens UWrapper a
ulens l f (UW a) = fmap (\x -> UW (set l x a)) (f (a ^. l))

instance Eq UWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((getSymb Equational a) ^. symbol) ((getSymb Equational b) ^. symbol)

instance Chunk UWrapper where
  id = ulens id
instance NamedIdea UWrapper where
  term = ulens term
  getA (UW a) = getA a
instance Quantity UWrapper where
  typ = ulens typ
  getSymb s  (UW a) = getSymb s a
  getUnit    (UW a) = getUnit a
  getStagedS (UW a) = getStagedS a
instance Unitary UWrapper where
  unit (UW a) = unit a
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
uw :: (Unitary c) => c -> UWrapper
uw = UW

-- | Unitary __and__ Concept Wrapper
data UCWrapper where
  UCW :: (Unitary c, Concept c) => c -> UCWrapper
  
uclens :: (forall c. (Unitary c, Concept c) => 
  Simple Lens c a) -> Simple Lens UCWrapper a
uclens l f (UCW a) = fmap (\x -> UCW (set l x a)) (f (a ^. l))

instance Eq UCWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Ord UCWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((getSymb Equational a) ^. symbol) ((getSymb Equational b) ^. symbol)

instance Chunk UCWrapper where
  id = uclens id
instance NamedIdea UCWrapper where
  term = uclens term
  getA (UCW a) = getA a
instance Quantity UCWrapper where
  typ = uclens typ
  getSymb s  (UCW a) = getSymb s a
  getUnit    (UCW a) = getUnit a
  getStagedS (UCW a) = getStagedS a
instance Unitary UCWrapper where
  unit (UCW a) = unit a
instance Concept UCWrapper where
  defn = uclens defn
  cdom = uclens cdom
  
-- | Constructor for Unital Wrappers. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
ucw :: (Unitary c, Concept c) => c -> UCWrapper
ucw = UCW

