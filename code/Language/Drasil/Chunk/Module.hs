{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Module(ModuleChunk(..), formatName, makeImpModule
  , makeUnimpModule, makeRecord, modcc, imp, hier, field, secret, uses, method) where

import Control.Lens (Simple, Lens, (^.), set)
import Data.List (intersperse)
import Data.Char (toUpper)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Wrapper (nw, NWrapper)

-- BEGIN METHODCHUNK --
data ModuleChunk where 
  MoC :: NamedIdea c => c -> Sentence -> Maybe c -> [VarChunk] -> 
    [MethodChunk] -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk

instance Chunk ModuleChunk where
  id = cl id

instance NamedIdea ModuleChunk where
  term = cl term

instance Eq ModuleChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

-- END METHODCHUNK --

cl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ModuleChunk a
cl l f (MoC a b c d e g h) = fmap (\x -> MoC (set l x a) b c d e g h) (f (a ^. l))

--Rebuild names for things because we removed the named record.
modcc :: ModuleChunk -> NWrapper
modcc (MoC c _ _ _ _ _ _) = nw c

secret :: ModuleChunk -> Sentence
secret (MoC _ s _ _ _ _ _) = s

imp :: ModuleChunk -> Maybe NWrapper
imp (MoC _ _ c _ _ _ _) = fmap nw c

field :: ModuleChunk -> [VarChunk]
field (MoC _ _ _ vs _ _ _) = vs

method :: ModuleChunk -> [MethodChunk]
method (MoC _ _ _ _ ms _ _) = ms

uses :: ModuleChunk -> [ModuleChunk]
uses (MoC _ _ _ _ _ us _ ) = us

hier :: ModuleChunk -> Maybe ModuleChunk
hier (MoC _ _ _ _ _ _ h) = h

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
