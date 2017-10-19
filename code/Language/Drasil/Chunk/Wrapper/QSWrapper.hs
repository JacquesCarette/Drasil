{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper.QSWrapper
  ( cqs, CQSWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Quantity

import Prelude hiding (id)

-- | Concept, Quantity, and Symbol Wrapper 
data CQSWrapper where
  CQS :: (Quantity c, Concept c) => c -> CQSWrapper
  
instance Chunk CQSWrapper where
  id = cqslens id
  
instance Eq CQSWrapper where
  a == b = (a ^. id) == (b ^. id)
  
instance Ord CQSWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((getSymb Equational a) ^. symbol) ((getSymb Equational b) ^. symbol)
  
instance NamedIdea CQSWrapper where
  term = cqslens term
  getA (CQS a) = getA a
  
instance Concept CQSWrapper where
  defn = cqslens defn
  cdom = cqslens cdom
  
instance Quantity CQSWrapper where
  getSymb s (CQS a) = getSymb s a
  getUnit (CQS a) = getUnit a
  typ = cqslens typ
  getStagedS (CQS a) = getStagedS a

-- | Constructor for CQSWrapper. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
cqs :: (Quantity c, Concept c) => c -> CQSWrapper
cqs = CQS
  
cqslens :: (forall c. (Quantity c, Concept c) => 
  Simple Lens c a) -> Simple Lens CQSWrapper a
cqslens l f (CQS a) = fmap (\x -> CQS (set l x a)) (f (a ^. l))
