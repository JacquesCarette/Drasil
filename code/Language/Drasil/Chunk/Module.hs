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
  imp :: Maybe ConceptChunk, method :: [MethodChunk], hier :: Maybe ModuleChunk}

instance Chunk ModuleChunk where
  name = cl . name

instance Concept ModuleChunk where
  descr = cl . descr

instance Eq ModuleChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk ConceptChunk
cl f (MoC a b c d e) = fmap (\x -> MoC x b c d e) (f a)


formatName :: ModuleChunk -> String
formatName m = (concat $ intersperse " " $
  map capitalize $ words (m ^. name)) ++ " Module"

makeImpModule :: ConceptChunk -> Sentence -> ConceptChunk -> [MethodChunk]
  -> Maybe ModuleChunk -> ModuleChunk
makeImpModule cc secret imp method hier = MoC cc secret (Just imp) method hier

makeUnimpModule :: ConceptChunk -> Sentence -> Maybe ModuleChunk -> ModuleChunk
makeUnimpModule cc secret hier = MoC cc secret Nothing [] hier
