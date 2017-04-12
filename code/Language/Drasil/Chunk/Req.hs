module Language.Drasil.Chunk.Req(ReqChunk(..)) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(term,getA))
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Wrapper (NWrapper)

-- BEGIN REQCHUNK --
data ReqChunk = ReqChunk
  { rCC :: NWrapper
  , rRelatedModules :: [ModuleChunk]
  }

instance Chunk ReqChunk where
  id = cl . id

instance NamedIdea ReqChunk where
  term = cl . term
  getA = getA . rCC
-- END REQCHUNK --

-- don't export this
cl :: Simple Lens ReqChunk NWrapper
cl f (ReqChunk a b) = fmap (\x -> ReqChunk x b) (f a)
