{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.MUChunk (MUChunk(..)) where

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Chunk (Chunk(..), Concept(..), SymbolForm(..))
import Language.Drasil.Unit (Unit(..)) --[S/Q] , Unit'(..)

data MUChunk where --May have Unit chunk
  Has :: (SymbolForm h, Unit h) => h -> MUChunk
  HasNot :: SymbolForm c => c -> MUChunk --Could accidentally add Unital

instance Chunk MUChunk where
  name = mulens name
  
instance Concept MUChunk where
  descr = mulens descr

instance SymbolForm MUChunk where
  symbol = mulens symbol

--[S/Q]  
--instance Unit' MUChunk where
--  unit' f (Has    h) = fmap (Has . maybe h (\t -> set unit t h)) (f $ Just $ h^.unit)
--  unit' f (HasNot h) = fmap (HasNot . maybe h (\_ -> h)) (f $ Nothing)

-- utilities which should not be exported
mulens :: (forall c. SymbolForm c => Simple Lens c a) -> Simple Lens MUChunk a
mulens l f (Has a) = fmap (\x -> Has (set l x a)) (f (a ^. l))
mulens l f (HasNot a) = fmap (\x -> HasNot (set l x a)) (f (a ^. l))
