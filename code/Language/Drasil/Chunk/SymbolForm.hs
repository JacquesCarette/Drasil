{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.SymbolForm 
  (SymbolForm(..), SF(..), SymbolChunk, sc) where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Symbol
import Prelude hiding (id)

-- | A SymbolForm is a 'Chunk' with a symbol that represents it
class Chunk c => SymbolForm c where
  symbol :: Simple Lens c Symbol
  
-- | SF is a wrapper for SymbolForms
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

-- | Symbols should be kept in chunks, so that we can remove the SymbolForm
-- instance from non-symbols.
data SymbolChunk = SC String Symbol

instance Chunk SymbolChunk where
  id f (SC i s) = fmap (\x -> SC x s) (f i)
instance Eq SymbolChunk where 
  -- This is the only case where we don't only match ids
  a == b = ((a ^. id) == (b ^. id)) || ((a ^. symbol) == (b ^. symbol))
instance SymbolForm SymbolChunk where
  symbol f (SC i s) = fmap (\x -> SC i x) (f s)

-- | Smart constructor for chunks for symbols
sc :: String -> Symbol -> SymbolChunk
sc = SC
