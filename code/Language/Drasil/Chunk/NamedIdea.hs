{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.NamedIdea where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Spec
import Language.Drasil.NounPhrase

import Prelude hiding (id)

class Chunk c => NamedIdea c where
  term :: Simple Lens c NP
  getA :: c -> Maybe Sentence
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

-- Get short form (if exists), else get term.
short :: NamedIdea c => c -> Sentence
short c = maybe (phrase (c^.term)) (\x -> x) (getA c)

-- === DATA TYPES/INSTANCES === --
data NamedChunk = NC String NP (Maybe Sentence)
instance Eq NamedChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk NamedChunk where
  id f (NC a b c) = fmap (\x -> NC x b c) (f a)
instance NamedIdea NamedChunk where
  term f (NC a b c) = fmap (\x -> NC a x c) (f b)
  getA (NC _ _ c) = c
  
nc :: String -> NP -> NamedChunk
nc i des = NC i des Nothing

nc' :: String -> NP -> String -> NamedChunk
nc' i t acc = NC i t (Just (S acc))

data NPNC where
  NPNC :: String -> Sentence -> (Maybe Sentence) -> NP -> NPNC
instance Eq NPNC where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk NPNC where
  id f (NPNC a b c d) = fmap (\x -> NPNC x b c d) (f a)
instance NamedIdea NPNC where
  term f (NPNC a b c d) = fmap (\x -> NPNC a b c x) (f d)
  getA (NPNC _ _ c _) = c
instance NounPhrase NPNC where
  phrase (NPNC _ _ _ d) = phrase d
  plural (NPNC _ _ _ d) = plural d
  sentenceCase (NPNC _ _ _ d) = sentenceCase d
  titleCase (NPNC _ _ _ d) = titleCase d
  
npnc :: String -> NP -> NPNC
npnc i n = NPNC i (phrase n) Nothing n

npnc' :: String -> NP -> String -> NPNC
npnc' i n a = NPNC i (phrase n) (Just $ S a) n

----------------------
-- various combinators
compoundterm :: (NamedIdea c, NounPhrase c, NamedIdea d, NounPhrase d) => 
  c -> d -> NamedChunk
compoundterm t1 t2 = NC (t1^.id ++ t2^.id) (compoundPhrase t1 t2) Nothing

compoundNPNC :: NPNC -> NPNC -> NPNC
compoundNPNC t1@(NPNC _ _ _ n1) t2@(NPNC _ _ _ n2) = 
  NPNC (t1^.id ++ t2^.id) (phrase $ compoundPhrase n1 n2) Nothing 
  (compoundPhrase n1 n2) 
  
-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2
--FIXME: This should be NamedIdea c & d, but temporarily swapped to NounPhrase
for :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (phrase $ t2 ^. term)

for' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for' t1 t2 = (short t1) +:+ S "for" +:+ (phrase $ t2 ^. term)

for'' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for'' t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (short t2)

for''' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for''' t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (plural $ t2 ^. term)

of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((phrase $ t1^.term) +:+ S "of" +:+ (plural $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))

of' :: (NounPhrase c, NounPhrase d) => c -> d -> NP
of' t1 t2 = nounPhrase'' 
  (phrase t1 +:+ S "of" +:+ plural t2)
  (phrase t1 +:+ S "of" +:+ plural t2)
  (Replace (at_start t1 +:+ S "of" +:+ plural t2))
  (Replace (titleize t1 +:+ S "of" +:+ titleize' t2))
  
with :: (NounPhrase c, NounPhrase d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (phrase t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (at_start t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize' t2))  
  
--Case of "T1s with T2", as opposed to the above "T1 with T2"
with' :: (NounPhrase c, NounPhrase d) => c -> d -> NP
with' t1 t2 = nounPhrase''
  (plural t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (at_start' t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize' t2))  
