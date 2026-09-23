module Drasil.System.Core (
  Purpose, Background, Scope, Motivation,
  SystemMeta,
  HasSystemMeta(..),
  mkSystemMeta,
) where

import Control.Lens ((^.), makeClassy)
import Data.Set qualified as S (unions)

import Drasil.Database (ChunkDB, HasUID(..), HasChunkRefs(..))
import Language.Drasil (Sentence, People)

import Drasil.System.ProjectName (ProjectName, HasProjectName(..))

-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of
-- README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

data SystemMeta = SystemMeta
  { _projName   :: ProjectName
  , _authors    :: People
  , _purpose    :: Purpose
  , _background :: Background
  , _scope      :: Scope
  , _motivation :: Motivation
  , _systemdb   :: ChunkDB
  }
makeClassy ''SystemMeta

instance HasUID SystemMeta where
  uid = projName . uid

instance HasProjectName SystemMeta where
  projectName = projName

instance HasChunkRefs SystemMeta where
  chunkRefs x = S.unions [
      chunkRefs (x ^. projName),
      chunkRefs (x ^. authors),
      chunkRefs (x ^. background),
      chunkRefs (x ^. scope),
      chunkRefs (x ^. motivation)
    ]

mkSystemMeta :: ProjectName -> People -> Purpose -> Background -> Scope ->
  Motivation -> ChunkDB -> SystemMeta
mkSystemMeta = SystemMeta
