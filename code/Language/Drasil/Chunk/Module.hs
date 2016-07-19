module Language.Drasil.Chunk.Module(ModuleChunk(..), makeImpModule
  , makeUnimpModule) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))

-- BEGIN METHODCHUNK --
data ModuleChunk = MoC { cc :: ConceptChunk, secret :: Sentence,
  imp :: Maybe String, method :: [MethodChunk] }

instance Chunk ModuleChunk where
  name = cl . name

instance Concept ModuleChunk where
  descr = cl . descr

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk ConceptChunk
cl f (MoC a b c d) = fmap (\x -> MoC x b c d) (f a)


makeImpModule :: ConceptChunk -> Sentence -> String -> [MethodChunk]
                -> ModuleChunk
makeImpModule cc secret imp method = MoC cc secret (Just imp) method

makeUnimpModule :: ConceptChunk -> Sentence -> ModuleChunk
makeUnimpModule cc secret = MoC cc secret Nothing []
