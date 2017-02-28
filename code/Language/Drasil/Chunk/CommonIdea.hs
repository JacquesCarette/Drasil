module Language.Drasil.Chunk.CommonIdea where

import Prelude hiding (id)

import Language.Drasil.Chunk (Chunk(id))
import Language.Drasil.Chunk.NamedIdea
import Control.Lens (Simple, Lens)
import Language.Drasil.Spec (Sentence)

class NamedIdea c => CommonIdea c where
  abrv :: Simple Lens c Sentence
  
data CI = CI String Sentence Sentence

instance Chunk CI where
  id f (CI a b c) = fmap (\x -> CI x b c) (f a)
instance NamedIdea CI where
  term f (CI a b c) = fmap (\x -> CI a x c) (f b)
  getA (CI _ _ c) = Just c
instance CommonIdea CI where
  abrv f (CI a b c) = fmap (\x -> CI a b x) (f c)

