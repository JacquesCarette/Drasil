{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.NamedIdea where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Spec

import Prelude hiding (id)

class Chunk c => NamedIdea c where
  term :: Simple Lens c Sentence
  getA :: c -> Maybe Sentence
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.
  short :: c -> Sentence -- Get short form (if exists), else get term. 
  
-- === DATA TYPES/INSTANCES === --
data NamedChunk = NC String Sentence (Maybe Sentence)
instance Eq NamedChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk NamedChunk where
  id f (NC a b c) = fmap (\x -> NC x b c) (f a)
instance NamedIdea NamedChunk where
  term f (NC a b c) = fmap (\x -> NC a x c) (f b)
  getA (NC _ _ c) = c
  short c@(NC _ _ Nothing) = c ^. term
  short (NC _ _ (Just s)) = s
  
nc :: String -> String -> NamedChunk
nc i des = NC i (S des) Nothing

nc' :: String -> String -> String -> NamedChunk
nc' i t acc = NC i (S t) (Just (S acc))

--Currently only used by RelationChunk and EqChunk
ncWDS :: String -> Sentence -> NamedChunk
ncWDS n d = NC n d Nothing

ncWDS' :: String -> Sentence -> String -> NamedChunk
ncWDS' i t a = NC i t (Just (S a))

----------------------
-- various combinators
compoundterm :: (NamedIdea c, NamedIdea d) => c -> d -> NamedChunk
compoundterm t1 t2 = NC (t1^.id ++ t2^.id) ((t1^.term) +:+ (t2^.term)) Nothing