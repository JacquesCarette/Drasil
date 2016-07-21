module Language.Drasil.Chunk.Module(ModuleChunk(..), formatName, makeImpModule
  , makeUnimpModule) where

import Control.Lens (Simple, Lens, (^.))
import Data.List (intersperse)

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Printing.Helpers

-- BEGIN METHODCHUNK --
data ModuleChunk = MoC { cc :: ConceptChunk, secret :: Sentence,
  imp :: Maybe ConceptChunk, method :: [MethodChunk] }

instance Chunk ModuleChunk where
  name = cl . name

instance Concept ModuleChunk where
  descr = cl . descr

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk ConceptChunk
cl f (MoC a b c d) = fmap (\x -> MoC x b c d) (f a)


formatName :: ModuleChunk -> String
formatName m = (concat $ intersperse " " $
  map capitalize $ words (m ^. name)) ++ " Module"

makeImpModule :: ConceptChunk -> Sentence -> ConceptChunk -> [MethodChunk]
                -> ModuleChunk
makeImpModule cc secret imp method = MoC cc secret (Just imp) method

makeUnimpModule :: ConceptChunk -> Sentence -> ModuleChunk
makeUnimpModule cc secret = MoC cc secret Nothing []
