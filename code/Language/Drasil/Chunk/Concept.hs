{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Concept where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Spec

import Prelude hiding (id)

class NamedIdea c => Concept c where
  defn :: Simple Lens c Sentence

-- === DATA TYPES === --
--- ConceptChunk ---  
data ConceptChunk where
  CC :: Concept c => NamedChunk -> Sentence -> [c] -> ConceptChunk 
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
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ConceptChunk a
nl l f (CC n d cd) = fmap (\x -> CC (set l x n) d cd) (f (n ^. l))
  
--FIXME: Temporary ConceptDomain tag hacking to not break everything.  
dcc :: String -> String -> String -> ConceptChunk 
dcc i ter des = CC (nc i ter) (S des) ([] :: [ConceptChunk])

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = CC (nc i t) d ([] :: [ConceptChunk])

ccStSS :: String -> Sentence -> Sentence -> ConceptChunk
ccStSS i t d = CC (ncs i t) d ([] :: [ConceptChunk])

dcc' :: String -> String -> String -> String -> ConceptChunk
dcc' i t d a = CC (nc' i t a) (S d) ([] :: [ConceptChunk])

ccs :: Concept c => NamedChunk -> Sentence -> [c] -> ConceptChunk --Explicit tagging
ccs = CC

cc :: NamedChunk -> String -> ConceptChunk
cc n d = CC n (S d) ([] :: [ConceptChunk])