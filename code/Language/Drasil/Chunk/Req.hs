module Language.Drasil.Chunk.Req(ReqChunk(..)) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module

-- BEGIN REQCHUNK --
data ReqChunk = ReqChunk
  { rCC :: ConceptChunk
  , rRelatedModules :: [ModuleChunk]
  }

instance Chunk ReqChunk where
  name = cl . name

instance Concept ReqChunk where
  descr = cl . descr
-- END REQCHUNK --

-- don't export this
cl :: Simple Lens ReqChunk ConceptChunk
cl f (ReqChunk a b) = fmap (\x -> ReqChunk x b) (f a)