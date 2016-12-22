module Language.Drasil.Chunk.Req(ReqChunk(..)) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module

-- BEGIN REQCHUNK --
data ReqChunk = ReqChunk
  { rCC :: NamedChunk
  , rRelatedModules :: [ModuleChunk]
  }

instance Chunk ReqChunk where
  id = cl . id

instance NamedIdea ReqChunk where
  term = cl . term
-- END REQCHUNK --

-- don't export this
cl :: Simple Lens ReqChunk NamedChunk
cl f (ReqChunk a b) = fmap (\x -> ReqChunk x b) (f a)