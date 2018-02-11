{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper.QSWrapper
  ( cqs, CQSWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import qualified Language.Drasil.Chunk.Quantity as Q

import Prelude hiding (id)

-- | Concept, Quantity, and Symbol Wrapper 
data CQSWrapper where
  CQS :: (Q.Quantity c, Concept c) => c -> CQSWrapper
  
instance Chunk CQSWrapper where
  id = cqslens id
  
instance Eq CQSWrapper where
  a == b = (a ^. id) == (b ^. id)
  
instance Ord CQSWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((Q.getSymb Equational a) ^. symbol) ((Q.getSymb Equational b) ^. symbol)
  
instance NamedIdea CQSWrapper where
  term = cqslens term

instance Idea CQSWrapper where
  getA (CQS a) = getA a
  
instance Concept CQSWrapper where
  defn = cqslens defn
  cdom = cqslens cdom
  
instance Q.Quantity CQSWrapper where
  getSymb s (CQS a) = Q.getSymb s a
  getUnit (CQS a) = Q.getUnit a
  typ = cqslens Q.typ
  getStagedS (CQS a) = Q.getStagedS a

-- | Constructor for CQSWrapper. Similar to 
-- 'Language.Drasil.Chunk.Wrapper.NWrapper' in its use
cqs :: (Q.Quantity c, Concept c) => c -> CQSWrapper
cqs = CQS
  
cqslens :: (forall c. (Q.Quantity c, Concept c) => 
  Simple Lens c a) -> Simple Lens CQSWrapper a
cqslens l f (CQS a) = fmap (\x -> CQS (set l x a)) (f (a ^. l))
