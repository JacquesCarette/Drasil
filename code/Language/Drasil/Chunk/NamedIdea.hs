{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.NamedIdea (
  NamedChunk, nc, IdeaDict, short, nw, mkIdea,
  compoundNC, compoundNC', compoundNC'', compoundNC''',
  the, theCustom) where

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
  
the :: (NamedIdea c) => c -> NamedChunk
the t = nc ("the" ++ t ^. uid) (nounPhrase'' 
  (S "the" +:+ (phrase $ t ^. term)) (S "the" +:+ (plural $ t ^. term))
  CapFirst CapWords)

theCustom :: (NamedIdea c) => (c -> Sentence) -> c -> NamedChunk
theCustom f t = nc ("the" ++ t ^. uid) (nounPhrase''(S "the" +:+ (f t)) 
  (S "the" +:+ (f t)) CapFirst CapWords)
