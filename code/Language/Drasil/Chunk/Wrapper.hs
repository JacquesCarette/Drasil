{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper 
  ( nw, cw
  , NWrapper, CWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Prelude hiding (id)

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
