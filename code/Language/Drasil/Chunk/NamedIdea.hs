module Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..),
  NamedChunk, nc, IdeaDict, compoundterm, short, nw, mkIdea,
  compoundNC, compoundNC', compoundNC'', compoundNC''',
  for, for', for'', of_, of_', of_'', of__, of'',
  with, with', and_, and_', andRT, the, theCustom, this, aNP, a_, ofA) where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Spec
import Language.Drasil.NounPhrase

-- | A NamedIdea is a 'term' that we've identified (has an 'id') as 
-- being worthy of naming.
class Chunk c => NamedIdea c where
  -- | Lens to the term (a noun phrase)
  term :: Simple Lens c NP

-- | An |Idea| is the 'meet' of |NamedIdea| and |CommonIdea|.
-- In other words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  getA :: c -> Maybe String
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

-- | Get short form (if it exists), else get term.
short :: Idea c => c -> Sentence
short c = maybe (phrase (c ^. term)) (\x -> x) (fmap S $ getA c)

-- === DATA TYPES/INSTANCES === --
-- | Note that a |NamedChunk| does not have an acronym/abbreviation
-- as that's a |CommonIdea|, which has its own representation
data NamedChunk = NC String NP

instance Eq        NamedChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance Chunk     NamedChunk where uid f (NC a b) = fmap (\x -> NC x b) (f a)
instance NamedIdea NamedChunk where term f (NC a b) = fmap (\x -> NC a x) (f b)
instance Idea      NamedChunk where getA (NC _ _) = Nothing
  
-- | 'NamedChunk' constructor, takes an uid and a term.
nc :: String -> NP -> NamedChunk
nc = NC

-- | |IdeaDict| is the canonical dictionary associated to the |Idea| class
-- don't export the record accessors
data IdeaDict = IdeaDict { _nc :: NamedChunk, _mabbr :: Maybe String }

instance Eq        IdeaDict where a == b = a ^. uid == b ^. uid
-- FIXME : this is not a good order!
instance Ord       IdeaDict where compare a b = compare (a ^. uid) (b ^. uid) 
instance Chunk     IdeaDict where uid = inc . uid
instance NamedIdea IdeaDict where term = inc . term
instance Idea      IdeaDict where getA (IdeaDict _ b) = b
  
inc :: Simple Lens IdeaDict NamedChunk
inc f (IdeaDict a b) = fmap (\x -> IdeaDict x b) (f a)

mkIdea :: String -> NP -> Maybe String -> IdeaDict
mkIdea s np ms = IdeaDict (nc s np) ms

-- Historical name: nw comes from 'named wrapped' from when
-- |NamedIdea| exported |getA| (now in |Idea|). But there are
-- no more wrappers, instead we have explicit dictionaries.
nw :: Idea c => c -> IdeaDict
nw c = IdeaDict (NC (c^.uid) (c^.term)) (getA c)

----------------------
-- various combinators

-- | Combinator for combining two 'NamedIdea's into one NamedChunk.
-- /Does not preserve abbreviations/
compoundterm :: (NamedIdea c, NamedIdea d) => c -> d -> NamedChunk
compoundterm t1 t2 = nc (t1^.uid ++ t2^.uid) (compoundPhrase (t1 ^. term) (t2 ^. term))

-- | Combinator for combining two 'NamedChunk's into one.
-- /Does not preserve abbreviations/
compoundNC :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC t1 t2 = nc 
  (t1^.uid ++ t2^.uid) (compoundPhrase (t1 ^. term) (t2 ^. term))
  
compoundNC' :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC' t1 t2 = nc 
  (t1^.uid ++ t2^.uid) (compoundPhrase'' plural plural (t1 ^. term) (t2 ^. term)) 
  
compoundNC'' :: (NamedIdea a, NamedIdea b) => 
  (NP -> Sentence) -> (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC'' f1 f2 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' f1 f2 (t1 ^. term) (t2 ^. term))

-- hack for Solution Characteristics Specification, calling upon plural will pluralize
-- Characteristics as it is the end of the first term (solutionCharacteristic)
compoundNC''' :: (NamedIdea a, NamedIdea b) => (NP -> Sentence) -> a -> b -> NamedChunk
compoundNC''' f1 t1 t2 = nc 
  (t1^.uid ++ t2^.uid) (compoundPhrase''' f1 (t1 ^. term) (t2 ^. term))

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
for' :: (NamedIdea c, Idea d) => c -> d -> Sentence
for' t1 t2 = (titleize $ t1 ^. term) +:+ S "for" +:+ (short t2)

-- | Similar to 'for', but allows one to specify the function to use on each term
-- before inserting for. For example one could use @for'' phrase plural t1 t2@
for'' :: (NamedIdea c, NamedIdea d) => (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
for'' f1 f2 t1 t2 = (f1 t1) +:+ S "for" +:+ (f2 t2)

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural is defaulted to @(phrase t1) "of" (plural t2)@
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((phrase $ t1^.term) +:+ S "of" +:+ (plural $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))

-- | Creates a noun phrase by combining two 'NamedIdea's with the word "of" between
-- them. 'phrase' is defaulted to @(phrase t1) "of" (plural t2)@. Plural is the same.
of_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_' t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (plural $ t2^.term))
  ((phrase $ t1^.term) +:+ S "of" +:+ (plural $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (plural $ t2^.term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize' $ t2 ^. term)))
  
of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((plural $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2^.term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))

of__ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of__ t1 t2 = nounPhrase'' 
  ((plural $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((plural $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  (Replace ((at_start' $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize' $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))

of'' :: (NamedIdea c, NamedIdea d) => (NP -> Sentence) -> (NP -> Sentence) -> c -> d -> Sentence
of'' f1 f2 t1 t2 = (f1 $ t1 ^. term) +:+ S "of" +:+ (f2 $ t2 ^. term)
  
-- | Similar to 'of\'', but with the word "with" instead of "of".
-- Phrase defaults to @(phrase t1) "with" (phrase t2)@, plural only pluralizes t2.
with :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (phrase (t1 ^. term) +:+ S "with" +:+ phrase (t2 ^. term))
  (plural (t1 ^. term) +:+ S "with" +:+ plural (t2 ^. term))
  (Replace (at_start (t1 ^. term) +:+ S "with" +:+ phrase (t2 ^. term)))
  (Replace (titleize' (t1 ^. term) +:+ S "with" +:+ titleize' (t2 ^. term)))  

-- | Similar to 'with', except this is the
-- case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- phrase defaults to @(plural t1) "with" (phrase t2)@, plural pluralizes both.
with' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with' t1 t2 = nounPhrase''
  (plural (t1 ^. term) +:+ S "with" +:+ phrase (t2 ^. term))
  (plural (t1 ^. term) +:+ S "with" +:+ plural (t2 ^. term))
  (Replace (at_start' (t1 ^. term) +:+ S "with" +:+ phrase (t2 ^. term)))
  (Replace (titleize' (t1 ^. term) +:+ S "with" +:+ titleize (t2 ^. term)))
  
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

andRT :: (NamedIdea c, NamedIdea d) => 
  (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
andRT f1 f2 t1 t2 = nounPhrase''
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (phrase $ t2 ^. term))
  ((phrase $ t1 ^. term) +:+ S "and" +:+ (plural $ t2 ^. term))
  (Replace ((at_start $ t1 ^. term) +:+ S "and" +:+ (phrase $ t2 ^. term)))
  (Replace ((f1 t1) +:+ S "and" +:+ (f2 t2)))
  
the :: (NamedIdea c) => c -> NamedChunk
the t = nc ("the" ++ t ^. uid) (nounPhrase'' 
  (S "the" +:+ (phrase $ t ^. term)) (S "the" +:+ (plural $ t ^. term))
  CapFirst CapWords)

theCustom :: (NamedIdea c) => (c -> Sentence) -> c -> NamedChunk
theCustom f t = nc ("the" ++ t ^. uid) (nounPhrase''(S "the" +:+ (f t)) 
  (S "the" +:+ (f t)) CapFirst CapWords)

this :: (NamedIdea c) => c -> NamedChunk
this t = nc ("this" ++ t ^. uid) (nounPhrase'' 
  (S "this" +:+ (phrase $ t ^. term)) (S "this" +:+ (plural $ t ^. term))
  CapFirst CapWords)

aNP :: (NamedIdea c) => c -> NP --Should not be allowed to pluralize
aNP t = nounPhrase'' 
  (S "a" +:+ (phrase $ t ^. term)) (S "a" +:+ (phrase $ t ^. term))
  CapFirst CapWords  
  
a_ :: (NamedIdea c) => c -> NamedChunk --Pluralization disallowed
a_ t = nc ("a" ++ t ^.uid) (nounPhrase'' 
  (S "a" +:+ (phrase $ t ^. term)) (S "a" +:+ (phrase $ t ^. term)) 
  CapFirst CapWords)

ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = nounPhrase'' 
  ((plural $ t1^.term) +:+ S "of a" +:+ (phrase $ t2^.term))
  ((plural $ t1^.term) +:+ S "of a" +:+ (phrase $ t2^.term))
  (Replace ((at_start' $ t1 ^. term) +:+ S "of a" +:+ (phrase $ t2 ^. term)))
  (Replace ((titleize' $ t1 ^. term) +:+ S "of a" +:+ (titleize $ t2 ^. term)))
