module Language.Drasil.Chunk.Module(ModuleChunk(..), makeModule) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))

-- BEGIN METHODCHUNK --
data ModuleChunk = MoC { cc :: ConceptChunk, secret :: Sentence, method :: [MethodChunk] }

instance Chunk ModuleChunk where
  name = cl . name

instance Concept ModuleChunk where
  descr = cl . descr

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk ConceptChunk
cl f (MoC a b c) = fmap (\x -> MoC x b c) (f a)


makeModule :: String -> Sentence -> Sentence -> [MethodChunk] -> ModuleChunk
makeModule nm desc secret methods = MoC (CC nm desc) secret methods
