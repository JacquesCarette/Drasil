{-# LANGUAGE GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Wrapper 
  ( nw, NWrapper
  ) where

import Control.Lens (Simple, Lens, set, (^.))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Prelude hiding (id)

-- | Idea Wrapper
data NWrapper where
  NW :: Idea c => c -> NWrapper
  
instance Chunk NWrapper where
  id = nlens id
instance Ord NWrapper where
  compare a b = compare (a ^. id) (b ^. id) --Not ideal?
  --compare a b = compare (abrv a) (abrv b)
instance NamedIdea NWrapper where
  term = nlens term
instance Idea NWrapper where
  getA (NW a) = getA a  
instance Eq NWrapper where
  a == b = (a ^. id) == (b ^. id)

--abrv (NC _ _ c) = c --> cannot acces due to being wrapped by NWrapper

-- | Idea Wrapper constructor.
-- Used to wrap different Ideas, so they will have the same type.
-- Essentially causes the system to ignore any information other than id, term,
-- and acronym (if exists)
nw :: Idea c => c -> NWrapper
nw = NW

nlens :: (forall c. (Idea c) => 
  Simple Lens c a) -> Simple Lens NWrapper a
nlens l f (NW a) = fmap (\x -> NW (set l x a)) (f (a ^. l))
