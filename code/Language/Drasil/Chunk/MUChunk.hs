{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.MUChunk (MUChunk(..)) where

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Chunk (Chunk(..), Concept(..), Quantity(..))
import Language.Drasil.Unit (Unit)

data MUChunk where --May have Unit chunk
  Has :: (Quantity h, Unit h) => h -> MUChunk
  HasNot :: Quantity c => c -> MUChunk --Could accidentally add Unital

instance Chunk MUChunk where
  name = mulens name
  
instance Concept MUChunk where
  descr = mulens descr

instance Quantity MUChunk where
  symbol = mulens symbol
  
-- instance Unit MUChunk where

mulens :: (forall c. Quantity c => Simple Lens c a) -> Simple Lens MUChunk a
mulens l f (Has a) = fmap (\x -> Has (set l x a)) (f (a ^. l))
mulens l f (HasNot a) = fmap (\x -> HasNot (set l x a)) (f (a ^. l))
