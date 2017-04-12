module Language.Drasil.Chunk.LC(LCChunk(..)) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(term,getA))
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Wrapper (NWrapper)

-- BEGIN LCCHUNK (likely change chunk) --
data LCChunk = LCChunk
  { lcCC :: NWrapper
  , lcRelatedModules :: [ModuleChunk]
  }

instance Chunk LCChunk where
  id = cl . id

instance NamedIdea LCChunk where
  term = cl . term
  getA = getA . lcCC
-- END LCCHUNK --

-- don't export this
cl :: Simple Lens LCChunk NWrapper
cl f (LCChunk a b) = fmap (\x -> LCChunk x b) (f a)
