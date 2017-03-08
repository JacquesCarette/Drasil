{-# Language Rank2Types #-}
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
data ConceptChunk = CC NamedChunk Sentence
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id = nl id
instance NamedIdea ConceptChunk where
  term = nl term
  getA (CC n _) = getA n
instance Concept ConceptChunk where
  defn f (CC n d) = fmap (\x -> CC n x) (f d)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ConceptChunk a
nl l f (CC n d) = fmap (\x -> CC (set l x n) d) (f (n ^. l))
  
makeDCC, dcc :: String -> String -> String -> ConceptChunk
makeDCC i ter des = CC (nc i ter) (S des)

dcc = makeDCC

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = CC (nc i t) d

ccStSS :: String -> Sentence -> Sentence -> ConceptChunk
ccStSS i t d = CC (ncs i t) d

dcc' :: String -> String -> String -> String -> ConceptChunk
dcc' i t d a = CC (nc' i t a) (S d)
