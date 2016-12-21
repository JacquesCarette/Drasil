module Language.Drasil.Chunk.LC(LCChunk(..)) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module

-- BEGIN LCCHUNK (likely change chunk) --
data LCChunk = LCChunk
  { lcCC :: ConceptChunk
  , lcRelatedModules :: [ModuleChunk]
  }

instance Chunk LCChunk where
  id = cl . id

instance NamedIdea LCChunk where
  term = cl . term
-- END LCCHUNK --

-- don't export this
cl :: Simple Lens LCChunk ConceptChunk
cl f (LCChunk a b) = fmap (\x -> LCChunk x b) (f a)