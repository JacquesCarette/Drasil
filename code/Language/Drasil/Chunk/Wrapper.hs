{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper (qc, CQSWrapper) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Prelude hiding (id)

data CQSWrapper where
  QC :: (SymbolForm c, Quantity c, Concept c) => c -> CQSWrapper

instance Chunk CQSWrapper where
  id = qclens id
  
instance NamedIdea CQSWrapper where
  term = qclens term
  
instance Concept CQSWrapper where
  defn = qclens defn

instance SymbolForm CQSWrapper where
  symbol = qclens symbol
  
instance Quantity CQSWrapper where
  getSymb = Just . SF
  getUnit (QC a) = getUnit a
  --FIXME: typ

qc :: (SymbolForm c, Quantity c, Concept c) => c -> CQSWrapper
qc = QC
  
qclens :: (forall c. (Quantity c, Concept c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens CQSWrapper a
qclens l f (QC a) = fmap (\x -> QC (set l x a)) (f (a ^. l))