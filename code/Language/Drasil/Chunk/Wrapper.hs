{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper 
  ( cqs, qs, nw, cw
  , CQSWrapper, QSWrapper, NWrapper, CWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Prelude hiding (id)

{- Concept, Quantity, and Symbol Wrapper -}
data CQSWrapper where
  CQS :: (SymbolForm c, Quantity c, Concept c) => c -> CQSWrapper
  
instance Chunk CQSWrapper where
  id = cqslens id
  
instance Eq CQSWrapper where
  a == b = (a ^. id) == (b ^. id)
  
instance NamedIdea CQSWrapper where
  term = cqslens term
  getA (CQS a) = getA a
  
instance Concept CQSWrapper where
  defn = cqslens defn
  
instance SymbolForm CQSWrapper where
  symbol = cqslens symbol
  
instance Quantity CQSWrapper where
  getSymb = Just . SF
  getUnit (CQS a) = getUnit a
  --FIXME: typ

cqs :: (SymbolForm c, Quantity c, Concept c) => c -> CQSWrapper
cqs = CQS
  
cqslens :: (forall c. (Quantity c, Concept c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens CQSWrapper a
cqslens l f (CQS a) = fmap (\x -> CQS (set l x a)) (f (a ^. l))

{- Quantity and Symbol Wrapper -}
data QSWrapper where
  QS :: (SymbolForm c, Quantity c) => c -> QSWrapper
  
instance Chunk QSWrapper where
  id = qslens id
  
instance Eq QSWrapper where
  a == b = (a ^. id) == (b ^. id)
  
instance NamedIdea QSWrapper where
  term = qslens term
  getA (QS a) = getA a
  
instance SymbolForm QSWrapper where
  symbol = qslens symbol
  
instance Quantity QSWrapper where
  getSymb = Just . SF
  getUnit (QS a) = getUnit a
  --FIXME: typ

qs :: (SymbolForm c, Quantity c) => c -> QSWrapper
qs = QS
  
qslens :: (forall c. (Quantity c, SymbolForm c) => 
  Simple Lens c a) -> Simple Lens QSWrapper a
qslens l f (QS a) = fmap (\x -> QS (set l x a)) (f (a ^. l))

{- NamedIdea Wrapper -}
data NWrapper where
  NW :: (NamedIdea c) => c -> NWrapper
  
instance Chunk NWrapper where
  id = nlens id
  
instance NamedIdea NWrapper where
  term = nlens term
  getA (NW a) = getA a

nw :: NamedIdea c => c -> NWrapper
nw = NW

nlens :: (forall c. (NamedIdea c) => 
  Simple Lens c a) -> Simple Lens NWrapper a
nlens l f (NW a) = fmap (\x -> NW (set l x a)) (f (a ^. l))

instance Eq NWrapper where
  a == b = (a ^. id) == (b ^. id)

{- Concept Wrapper -}
data CWrapper where
  CW :: (Concept c) => c -> CWrapper
  
instance Chunk CWrapper where
  id = clens id
  
instance NamedIdea CWrapper where
  term = clens term
  getA (CW a) = getA a
  
instance Concept CWrapper where
  defn = clens defn

cw :: Concept c => c -> CWrapper
cw = CW

clens :: (forall c. (Concept c) => 
  Simple Lens c a) -> Simple Lens CWrapper a
clens l f (CW a) = fmap (\x -> CW (set l x a)) (f (a ^. l))

instance Eq CWrapper where
 a == b = (a ^. id) == (b ^. id)