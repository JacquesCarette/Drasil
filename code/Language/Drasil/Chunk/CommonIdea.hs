module Language.Drasil.Chunk.CommonIdea where

import Language.Drasil.Chunk.NamedIdea
import Control.Lens (Simple, Lens)
import Language.Drasil.Spec

class NamedIdea c => CommonIdea c where
  abrv :: Simple Lens c Sentence
  
