module Language.Drasil.Chunk.Req(ReqChunk(..), emptyN) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(term,getA), npnc)
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Wrapper (NWrapper)
import Language.Drasil.NounPhrase (cn)
import Language.Drasil.Chunk.Wrapper (nw)

-- BEGIN REQCHUNK --
-- | Requirement Chunks contain a NamedIdea (as an NWrapper) and a list of
-- related modules
data ReqChunk = ReqChunk
  { rNI :: NWrapper
  , rRelatedModules :: [ModuleChunk]
  }

instance Chunk ReqChunk where
  id = cl . id

instance NamedIdea ReqChunk where
  term = cl . term
  getA = getA . rNI
-- END REQCHUNK --

-- don't export this
cl :: Simple Lens ReqChunk NWrapper
cl f (ReqChunk a b) = fmap (\x -> ReqChunk x b) (f a)

emptyN :: NWrapper
emptyN = nw $ npnc "" (cn "")
