module Language.Drasil.Chunk.Module(ModuleChunk(..), formatName, makeImpModule
  , makeUnimpModule, makeRecord) where

import Control.Lens (Simple, Lens, (^.))
import Data.List (intersperse)
import Data.Char (toUpper)

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))

-- BEGIN METHODCHUNK --
data ModuleChunk = MoC { modcc :: ConceptChunk, secret :: Sentence,
  imp :: Maybe ConceptChunk, field :: [VarChunk],
  method :: [MethodChunk], uses :: [ModuleChunk], hier :: Maybe ModuleChunk }

instance Chunk ModuleChunk where
  name = cl . name

instance Concept ModuleChunk where
  descr = cl . descr

instance Eq ModuleChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk ConceptChunk
cl f (MoC a b c d e g h) = fmap (\x -> MoC x b c d e g h) (f a)


formatName :: ModuleChunk -> String
formatName m = (concat $ intersperse " " $
  map capFirst $ words (m ^. name)) ++ " Module"
  where capFirst [] = []
        capFirst (c:cs) = toUpper c:cs

makeRecord :: ConceptChunk -> Sentence -> ConceptChunk -> [VarChunk]
  -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeRecord cc' secret' imp' field' uses' hier' =
  MoC cc' secret' (Just imp') field' [] uses' hier'

makeImpModule :: ConceptChunk -> Sentence -> ConceptChunk -> [VarChunk]
  -> [MethodChunk] -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeImpModule cc' secret' imp' field' method' uses' hier' =
  MoC cc' secret' (Just imp') field' method' uses' hier'

makeUnimpModule :: ConceptChunk -> Sentence -> Maybe ModuleChunk -> ModuleChunk
makeUnimpModule cc' secret' hier' = MoC cc' secret' Nothing [] [] [] hier'
