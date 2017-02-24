{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Concept where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea

import Control.Lens (Simple, Lens, (^.), set)

import Language.Drasil.Spec

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

--- CONVAR ---
  
--FIXME: This is a temporary data structure created to advance the chunk
--  hierarchy redesign. A full overhaul of datastructures is coming soon.

data ConVar = CV { _con :: ConceptChunk
                 , _symb :: Symbol
                 , _typ :: Space }
                     
instance Eq ConVar where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConVar where
  id = cvl . id
instance NamedIdea ConVar where
  term = cvl . term
  getA (CV c _ _) = getA c
instance Concept ConVar where
  defn = cvl . defn
instance SymbolForm ConVar where
  symbol f (CV c s t) = fmap (\x -> CV c x t) (f s)

--FIXME: This should not be exported.
cvl :: Simple Lens ConVar ConceptChunk
cvl f (CV c s t) = fmap (\x -> CV x s t) (f c)

cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv = CV

--FIXME: Remove this hack
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c s Rational