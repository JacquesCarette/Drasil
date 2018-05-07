{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.NamedIdea (
  NamedChunk, nc, IdeaDict, compoundterm, short, nw, mkIdea,
  compoundNC, compoundNC', compoundNC'', compoundNC''',
  for, for', for'', the, theCustom) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA))
import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Spec
import Language.Drasil.NounPhrase

-- | Get short form (if it exists), else get term.
short :: Idea c => c -> Sentence
short c = maybe (phrase (c ^. term)) id (fmap S $ getA c)

-- === DATA TYPES/INSTANCES === --
-- | Note that a |NamedChunk| does not have an acronym/abbreviation
-- as that's a |CommonIdea|, which has its own representation
data NamedChunk = NC {_uu :: String, _np :: NP}
makeLenses ''NamedChunk

instance Eq        NamedChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID    NamedChunk where uid = uu
instance NamedIdea NamedChunk where term = np
instance Idea      NamedChunk where getA = \_ -> Nothing
  
-- | 'NamedChunk' constructor, takes an uid and a term.
nc :: String -> NP -> NamedChunk
nc = NC

-- | |IdeaDict| is the canonical dictionary associated to |Idea|
-- don't export the record accessors
data IdeaDict = IdeaDict { _nc' :: NamedChunk, _mabbr :: Maybe String }
makeLenses ''IdeaDict

instance Eq        IdeaDict where a == b = a ^. uid == b ^. uid
instance HasUID    IdeaDict where uid = nc' . uid
instance NamedIdea IdeaDict where term = nc' . term
instance Idea      IdeaDict where getA = view mabbr
  
mkIdea :: String -> NP -> Maybe String -> IdeaDict
mkIdea s np' ms = IdeaDict (nc s np') ms

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
  
the :: (NamedIdea c) => c -> NamedChunk
the t = nc ("the" ++ t ^. uid) (nounPhrase'' 
  (S "the" +:+ (phrase $ t ^. term)) (S "the" +:+ (plural $ t ^. term))
  CapFirst CapWords)

theCustom :: (NamedIdea c) => (c -> Sentence) -> c -> NamedChunk
theCustom f t = nc ("the" ++ t ^. uid) (nounPhrase''(S "the" +:+ (f t)) 
  (S "the" +:+ (f t)) CapFirst CapWords)