{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Concept where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Spec

import Prelude hiding (id)

class NamedIdea c => Concept c where
  defn :: Simple Lens c Sentence
  cdom :: Simple Lens c [CWrapper] --This should be exported for use by the
              --Drasil framework, but should not be exported beyond that.

-- === DATA TYPES === --
--- ConceptChunk ---  
data ConceptChunk where
  CC :: NamedChunk -> Sentence -> [CWrapper] -> ConceptChunk 
  --[c] is the ConceptDomain(s)
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id = nl id
instance NamedIdea ConceptChunk where
  term = nl term
  getA (CC n _ _) = getA n
instance Concept ConceptChunk where
  defn f (CC n d cd) = fmap (\x -> CC n x cd) (f d)
  cdom f (CC n d cd) = fmap (\x -> CC n d x) (f cd)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ConceptChunk a
nl l f (CC n d cd) = fmap (\x -> CC (set l x n) d cd) (f (n ^. l))

--FIXME: Temporary ConceptDomain tag hacking to not break everything.  
dcc :: String -> String -> String -> ConceptChunk 
dcc i ter des = CC (nc i ter) (S des) ([] :: [CWrapper])

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = CC (nc i t) d ([] :: [CWrapper])

ccStSS :: String -> Sentence -> Sentence -> ConceptChunk
ccStSS i t d = CC (ncs i t) d ([] :: [CWrapper])

dcc' :: String -> String -> String -> String -> ConceptChunk
dcc' i t d a = CC (nc' i t a) (S d) ([] :: [CWrapper])

ccs :: NamedChunk -> Sentence -> [CWrapper] -> ConceptChunk --Explicit tagging
ccs = CC

cc :: NamedChunk -> String -> ConceptChunk
cc n d = CC n (S d) ([] :: [CWrapper])

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
  cdom = clens cdom

cw :: Concept c => c -> CWrapper
cw = CW

clens :: (forall c. (Concept c) => 
  Simple Lens c a) -> Simple Lens CWrapper a
clens l f (CW a) = fmap (\x -> CW (set l x a)) (f (a ^. l))

instance Eq CWrapper where
 a == b = (a ^. id) == (b ^. id)