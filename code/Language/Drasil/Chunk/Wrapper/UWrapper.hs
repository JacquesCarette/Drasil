{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper.UWrapper
  ( uw
  , UWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Unital
import Prelude hiding (id)

{- Unital Wrapper -}
data UWrapper where
  UW :: (Unital c, SymbolForm c) => c -> UWrapper
  
ulens :: (forall c. (Unital c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens UWrapper a
ulens l f (UW a) = fmap (\x -> UW (set l x a)) (f (a ^. l))

instance Chunk UWrapper where
  id = ulens id
instance NamedIdea UWrapper where
  term = ulens term
  getA (UW a) = getA a
instance Quantity UWrapper where
  typ = ulens typ
  getSymb (UW a) = getSymb a
  getUnit (UW a) = getUnit a
instance Unital UWrapper where
  unit (UW a) = unit a
instance SymbolForm UWrapper where
  symbol = ulens symbol
instance Concept UWrapper where
  defn = ulens defn
  
uw :: (Unital c, SymbolForm c) => c -> UWrapper
uw = UW
