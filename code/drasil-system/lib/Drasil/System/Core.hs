module Drasil.System.Core (
  Purpose, Background, Scope, Motivation,
  SystemMeta,
  HasSystemMeta(..),
  projRepoName, projHRName,
  mkSystemMeta,
) where

import Control.Lens (Lens', makeClassy)

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence, People, CI)

import Drasil.System.ProjectName (ProjectName, repo, humanReadable)

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
  , _sysName    :: CI -- FIXME: All usage of `sysName` should be removed in favour of `projName`.
  , _authors    :: People
  , _purpose    :: Purpose
  , _background :: Background
  , _scope      :: Scope
  , _motivation :: Motivation
  , _systemdb   :: ChunkDB
  }

makeClassy ''SystemMeta

-- | Lens to access the repository name from any structure with 'HasSystemMeta'.
projRepoName :: HasSystemMeta a => Lens' a String
projRepoName = projName . repo

-- | Lens to access the human-readable project name from any structure with 'HasSystemMeta'.
projHRName :: HasSystemMeta a => Lens' a String
projHRName = projName . humanReadable

mkSystemMeta :: ProjectName -> CI -> People -> Purpose -> Background -> Scope ->
  Motivation -> ChunkDB -> SystemMeta
mkSystemMeta = SystemMeta
