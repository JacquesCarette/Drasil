{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.MUChunk (MUChunk(..)) where

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Chunk (Chunk(..), Concept(..), Quantity(..))
import Language.Drasil.Chunk.Unital (UnitalChunk(..))

data MUChunk where --May have Unit chunk
  Has :: UnitalChunk -> MUChunk
  HasNot :: Quantity c => c -> MUChunk --Could accidentally add Unital

instance Chunk MUChunk where
  name = q . name
  
instance Concept MUChunk where
  descr = q . descr

instance Quantity MUChunk where
  symbol = q . symbol
  
-- instance Unit MUChunk where

  
-- HELPERS --  
qlens :: (forall c. Quantity c => Simple Lens c a) -> Simple Lens Q a
qlens l f (Q a) = fmap (\x -> Q (set l x a)) (f (a ^. l))

data Q where
  Q :: Quantity c => c -> Q
  
instance Chunk Q where 
  name = qlens name

instance Concept Q where 
  descr = qlens descr

instance Quantity Q where
  symbol = qlens symbol

q :: Simple Lens MUChunk Q
q f (HasNot a) = fmap (\(Q x) -> HasNot x) (f (Q a))
q f (Has (UC a b)) = fmap (\(Q x) -> (Has (UC x b))) (f (Q a))
