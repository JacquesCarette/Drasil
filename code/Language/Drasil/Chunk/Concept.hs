module Language.Drasil.Chunk.Concept where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea

import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Spec

import Prelude hiding (id)

class NamedIdea c => Concept c where
  defn :: Simple Lens c Sentence

-- === DATA TYPES === --

--- ConceptChunk ---  
data ConceptChunk = DCC String Sentence Sentence (Maybe Sentence)
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id f (DCC n t d a) = fmap (\x -> DCC x t d a) (f n)
instance NamedIdea ConceptChunk where
  term f (DCC n t d a) = fmap (\x -> DCC n x d a) (f t)
  getA (DCC _ _ _ a) = a
instance Concept ConceptChunk where
  defn f (DCC n t d a) = fmap (\x -> DCC n t x a) (f d)
  
makeDCC, dcc :: String -> String -> String -> ConceptChunk
makeDCC i ter des = DCC i (S ter) (S des) Nothing

dcc = makeDCC

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = DCC i (S t) d Nothing

ccStSS :: String -> Sentence -> Sentence -> ConceptChunk
ccStSS i t d = DCC i t d Nothing

dcc' :: String -> String -> String -> String -> ConceptChunk
dcc' i t d a = DCC i (S t) (S d) (Just (S a))
