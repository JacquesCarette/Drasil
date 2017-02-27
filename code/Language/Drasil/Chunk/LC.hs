module Language.Drasil.Chunk.LC(LCChunk(..)) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(term,getA), NamedChunk)
import Language.Drasil.Chunk.Module

-- BEGIN LCCHUNK (likely change chunk) --
data LCChunk = LCChunk
  { lcCC :: NamedChunk
  , lcRelatedModules :: [ModuleChunk]
  }

instance Chunk LCChunk where
  id = cl . id

instance NamedIdea LCChunk where
  term = cl . term
  getA = getA . lcCC
-- END LCCHUNK --

-- don't export this
cl :: Simple Lens LCChunk NamedChunk
cl f (LCChunk a b) = fmap (\x -> LCChunk x b) (f a)
