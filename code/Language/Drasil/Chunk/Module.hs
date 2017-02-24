{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Module(ModuleChunk(..), formatName, makeImpModule
  , makeImpModuleNoGen, makeUnimpModule, makeRecord, modcc, imp, hier, field
  , secret, uses, method, generated) where

import Control.Lens (Simple, Lens, (^.), set)
import Data.List (intersperse)
import Data.Char (toUpper)
import Prelude hiding (id)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea, term)
import Language.Drasil.Chunk.VarChunk (VarChunk)
import Language.Drasil.Chunk.Method
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Wrapper (nw, NWrapper)

-- BEGIN METHODCHUNK --
-- (Currently) used for module guide, MIS and code generation
data ModuleChunk where 
  MoC :: NWrapper          -- Name
      -> Sentence          -- Secret
      -> Maybe NWrapper    -- what implements this, if at all
      -> [VarChunk]        -- module fields, aka state variables
      -> [MethodChunk]     -- the methods offered by module
      -> [ModuleChunk]     -- what modules this one depends on [extract!]
      -> Maybe ModuleChunk -- Parent module, for documents [extract!]
      -> Bool              -- Should this module be generated?
      -> ModuleChunk

instance Chunk ModuleChunk where
  id = cl id

instance NamedIdea ModuleChunk where
  term = cl term

instance Eq ModuleChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

-- END METHODCHUNK --

cl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens ModuleChunk a
cl l f (MoC a b c d e g h i) = fmap (\x -> MoC (set l x a) b c d e g h i) (f (a ^. l))

--Rebuild names for things because we removed the named record.
modcc :: ModuleChunk -> NWrapper
modcc (MoC c _ _ _ _ _ _ _) = c

secret :: ModuleChunk -> Sentence
secret (MoC _ s _ _ _ _ _ _) = s

imp :: ModuleChunk -> Maybe NWrapper
imp (MoC _ _ c _ _ _ _ _) = c

field :: ModuleChunk -> [VarChunk]
field (MoC _ _ _ vs _ _ _ _) = vs

method :: ModuleChunk -> [MethodChunk]
method (MoC _ _ _ _ ms _ _ _) = ms

uses :: ModuleChunk -> [ModuleChunk]
uses (MoC _ _ _ _ _ us _ _) = us

hier :: ModuleChunk -> Maybe ModuleChunk
hier (MoC _ _ _ _ _ _ h _) = h

generated :: ModuleChunk -> Bool
generated (MoC _ _ _ _ _ _ _ b) = b

formatName :: ModuleChunk -> String
formatName m = (concat $ intersperse " " $
  map capFirst $ words (m ^. id)) ++ " Module"
  where capFirst [] = []
        capFirst (c:cs) = toUpper c:cs

makeRecord :: (NamedIdea c1, NamedIdea c2) => c1 -> Sentence -> c2 -> [VarChunk]
  -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeRecord cc' secret' imp' field' uses' hier' =
  MoC (nw cc') secret' (Just (nw imp')) field' [] uses' hier' True

makeImpModule :: (NamedIdea c1, NamedIdea c2) => c1 -> Sentence -> c2 -> [VarChunk]
  -> [MethodChunk] -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeImpModule cc' secret' imp' field' method' uses' hier' =
  MoC (nw cc') secret' (Just (nw imp')) field' method' uses' hier' True

makeImpModuleNoGen :: (NamedIdea c1, NamedIdea c2) => c1 -> Sentence -> c2 -> [VarChunk]
  -> [MethodChunk] -> [ModuleChunk] -> Maybe ModuleChunk -> ModuleChunk
makeImpModuleNoGen cc' secret' imp' field' method' uses' hier' =
  MoC (nw cc') secret' (Just (nw imp')) field' method' uses' hier' False

makeUnimpModule :: NamedIdea c => c -> Sentence -> Maybe ModuleChunk -> ModuleChunk
makeUnimpModule cc' secret' hier' = MoC (nw cc') secret' Nothing [] [] [] hier'
  False
