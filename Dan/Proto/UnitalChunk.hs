{-# LANGUAGE GADTs, Rank2Types #-}
module UnitalChunk (UnitalChunk(..)) where

import Chunk (Chunk(..), Concept(..), Quantity(..))
import Unit (Unit(..), UnitDefn(..))

import Control.Lens (Simple, Lens, (^.), set)

data UnitalChunk where
  UC :: (Quantity c, Unit u) => c -> u -> UnitalChunk

data Q where
  Q :: Quantity c => c -> Q

qlens :: (forall c. Quantity c => Simple Lens c a) -> Simple Lens Q a
qlens l f (Q a) = fmap (\x -> Q (set l x a)) (f (a ^. l))

instance Chunk Q where 
  name = qlens name

instance Concept Q where 
  descr = qlens descr

instance Quantity Q where
  symbol = qlens symbol

-- these don't get exported
q :: Simple Lens UnitalChunk Q
q f (UC a b) = fmap (\(Q x) -> UC x b) (f (Q a))

u :: Simple Lens UnitalChunk UnitDefn
u f (UC a b) = fmap (\(UU x) -> UC a x) (f (UU b))

instance Chunk UnitalChunk where
  name = q . name

instance Concept UnitalChunk where
  descr = q . descr

instance Quantity UnitalChunk where
  symbol = q . symbol

instance Unit UnitalChunk where
  unit = u . unit
