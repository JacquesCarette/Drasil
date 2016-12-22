module Language.Drasil.Chunk.Module(ModuleChunk(..), formatName, makeImpModule
  , makeUnimpModule, makeRecord) where

import Control.Lens (Simple, Lens, (^.))
import Data.List (intersperse)
import Data.Char (toUpper)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))

-- BEGIN METHODCHUNK --
data ModuleChunk = MoC { modcc :: NamedChunk, secret :: Sentence,
  imp :: Maybe NamedChunk, field :: [VarChunk],
  method :: [MethodChunk], uses :: [ModuleChunk], hier :: Maybe ModuleChunk }

instance Chunk ModuleChunk where
  id = cl . id

instance NamedIdea ModuleChunk where
  term = cl . term

instance Eq ModuleChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

-- END METHODCHUNK --

cl ::  Simple Lens ModuleChunk NamedChunk
cl f (MoC a b c d e g h) = fmap (\x -> MoC x b c d e g h) (f a)


formatName :: ModuleChunk -> String
formatName m = (concat $ intersperse " " $
  map capFirst $ words (m ^. id)) ++ " Module"
  where capFirst [] = []
        capFirst (c:cs) = toUpper c:cs

makeRecord :: NamedChunk -> Sentence -> NamedChunk -> [VarChunk]
  -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeRecord cc' secret' imp' field' uses' hier' =
  MoC cc' secret' (Just imp') field' [] uses' hier'

makeImpModule :: NamedChunk -> Sentence -> NamedChunk -> [VarChunk]
  -> [MethodChunk] -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeImpModule cc' secret' imp' field' method' uses' hier' =
  MoC cc' secret' (Just imp') field' method' uses' hier'

makeUnimpModule :: NamedChunk -> Sentence -> Maybe ModuleChunk -> ModuleChunk
makeUnimpModule cc' secret' hier' = MoC cc' secret' Nothing [] [] [] hier'
