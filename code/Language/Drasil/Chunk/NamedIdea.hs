{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.NamedIdea where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Spec
import Language.Drasil.NounPhrase

import Prelude hiding (id)

-- | A NamedIdea is a 'Chunk' (has 'id'), which also has a 'term'
-- and /may/ have an accronym/abbreviation.
class Chunk c => NamedIdea c where
  -- | Lens to the term (a noun phrase)
  term :: Simple Lens c NP
  -- | Provides (Just abbreviation) or Nothing if it does not exist
  getA :: c -> Maybe Sentence
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

-- | Get short form (if it exists), else get term.
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
  
-- | 'NamedChunk' constructor, takes an id and a term. For NamedChunks
-- without abbreviations
nc :: String -> NP -> NamedChunk
nc i des = NC i des Nothing

-- | 'NamedChunk' constructor for NamedChunks with abbreviations
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

-- | 'NPNC' constructor for those without abbreviations
npnc :: String -> NP -> NPNC
npnc i n = NPNC i (phrase n) Nothing n

-- | 'NPNC' constructor for those with abbreviations
npnc' :: String -> NP -> String -> NPNC
npnc' i n a = NPNC i (phrase n) (Just $ S a) n

----------------------
-- various combinators

-- | Combinator for combining two 'NamedIdea's into one NamedChunk.
-- /Does not preserve abbreviations/
compoundterm :: (NamedIdea c, NamedIdea d) => 
  c -> d -> NamedChunk
compoundterm t1 t2 = 
  NC (t1^.id ++ t2^.id) (compoundPhrase (t1 ^. term) (t2 ^. term)) Nothing

-- | Combinator for combining two 'NPNC's into one.
-- /Does not preserve abbreviations/
compoundNPNC :: (NamedIdea a, NamedIdea b) => a -> b -> NPNC
compoundNPNC t1 t2 = NPNC 
  (t1^.id ++ t2^.id) (phrase $ compoundPhrase (t1 ^. term) (t2 ^. term)) Nothing 
  (compoundPhrase (t1 ^. term) (t2 ^. term))
  
compoundNPNC' :: (NamedIdea a, NamedIdea b) => a -> b -> NPNC
compoundNPNC' t1 t2 = NPNC 
  (t1^.id ++ t2^.id) (phrase $ compoundPhrase (t1 ^. term) (t2 ^. term)) Nothing 
  (compoundPhrase'' (t1 ^. term) (t2 ^. term)) 
  
-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2

-- | Inserts the word "for" between the titleized versions of
-- two terms
for :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (titleize $ t2 ^. term)

-- | Similar to 'for', but uses titleized version of term 1 with the abbreviation
-- (if it exists, phrase otherwise) for term 2
for' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
for' t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (short t2)

-- | Similar to 'for', but allows one to specify the function to use on each term
-- before inserting for. For example one could use @for'' phrase plural t1 t2@
for'' :: (NamedIdea c, NamedIdea d) => (NP -> Sentence) -> (NP -> Sentence) -> c -> d -> Sentence
for'' f1 f2 t1 t2 = (f1 $ t1 ^. term) +:+ S "for" +:+ (f2 $ t2 ^. term)

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((phrase $ t1^.term) +:+ S "of" +:+ (plural $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))

--FIXME: This should be NamedIdea c & d, but temporarily swapped to NounPhrase
-- | Creates a noun phrase by combining two 'NounPhrase's with the word "of" between
-- them. 'phrase' is defaulted to @(phrase t1) "of" (plural t2)@. Plural is the same.
of' :: (NounPhrase c, NounPhrase d) => c -> d -> NP
of' t1 t2 = nounPhrase'' 
  (phrase t1 +:+ S "of" +:+ plural t2)
  (phrase t1 +:+ S "of" +:+ plural t2)
  (Replace (at_start t1 +:+ S "of" +:+ plural t2))
  (Replace (titleize t1 +:+ S "of" +:+ titleize' t2))
  
--FIXME: This should be NamedIdea c & d, but temporarily swapped to NounPhrase
-- | Similar to 'of\'', but with the word "with" instead of "of".
-- Phrase defaults to @(phrase t1) "with" (phrase t2)@, plural only pluralizes t2.
with :: (NounPhrase c, NounPhrase d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (phrase t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (at_start t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize' t2))  

--FIXME: This should be NamedIdea c & d, but temporarily swapped to NounPhrase  
-- | Similar to 'with', except this is the
-- case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- phrase defaults to @(plural t1) "with" (phrase t2)@, plural pluralizes both.

with' :: (NounPhrase c, NounPhrase d) => c -> d -> NP
with' t1 t2 = nounPhrase''
  (plural t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ plural t2)
  (Replace (at_start' t1 +:+ S "with" +:+ phrase t2))
  (Replace (titleize' t1 +:+ S "with" +:+ titleize' t2))
  
and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = nounPhrase''
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (phrase $ t2 ^. term))
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (plural $ t2 ^. term))
  (Replace ((at_start $ t1 ^. term) +:+ S "and" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "and" +:+ (titleize $ t2 ^. term)))

and_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_' t1 t2 = nounPhrase'' 
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (plural $ t2 ^. term))
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (plural $ t2 ^. term))
  (Replace ((at_start $ t1 ^. term) +:+ S "and" +:+ (plural $ t2 ^. term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "and" +:+ (titleize' $ t2 ^. term)))


the :: (NamedIdea c) => c -> NP
the t = nounPhrase'' 
  (S "the" +:+ (phrase $ t ^. term)) (S "the" +:+ (plural $ t ^. term))
  CapFirst CapWords

aNP :: (NamedIdea c) => c -> NP --Should not be allowed to pluralize
aNP t = nounPhrase'' 
  (S "a" +:+ (phrase $ t ^. term)) (S "a" +:+ (phrase $ t ^. term))
  CapFirst CapWords  
  