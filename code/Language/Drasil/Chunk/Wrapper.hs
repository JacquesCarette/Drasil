{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper (qc, QWrapper) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Prelude hiding (id)

data QWrapper where
  QC :: (SymbolForm c, Quantity c) => c -> QWrapper

instance Chunk QWrapper where
  id = qclens id
  
instance NamedIdea QWrapper where
  term = qclens term

instance SymbolForm QWrapper where
  symbol = qclens symbol
  
instance Quantity QWrapper where
  getSymb = Just . SF
  getUnit (QC a) = getUnit a
  --FIXME: typ

qc :: (SymbolForm c, Quantity c) => c -> QWrapper
qc = QC
  
qclens :: (forall c. SymbolForm c => Simple Lens c a) -> Simple Lens QWrapper a
qclens l f (QC a) = fmap (\x -> QC (set l x a)) (f (a ^. l))