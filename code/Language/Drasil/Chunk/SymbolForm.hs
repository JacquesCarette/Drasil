{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.SymbolForm where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Symbol
import Prelude hiding (id)

class Chunk c => SymbolForm c where
  symbol :: Simple Lens c Symbol
  
data SF where 
  SF :: SymbolForm c => c -> SF
instance Chunk SF where
  id = sfl id
instance SymbolForm SF where
  symbol = sfl symbol
instance Eq SF where
  (SF s1) == (SF s2) = (s1 ^. id) == (s2 ^. id)

sfl :: (forall c. (SymbolForm c) => Simple Lens c a) -> Simple Lens SF a
sfl l f (SF a) = fmap (\x -> SF (set l x a)) (f (a ^. l))