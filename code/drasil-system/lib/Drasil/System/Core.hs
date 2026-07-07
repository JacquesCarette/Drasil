module Drasil.System.Core (
  Purpose, Background, Scope, Motivation,
  SystemMeta,
  HasSystemMeta(..),
  mkSystemMeta,
) where

import Control.Lens (makeClassy)

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence, People, CI)

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
  { _sysName    :: CI -- FIXME: This should not be a CI.
  , _authors    :: People
  , _purpose    :: Purpose
  , _background :: Background
  , _scope      :: Scope
  , _motivation :: Motivation
  , _systemdb   :: ChunkDB
  }

makeClassy ''SystemMeta

mkSystemMeta :: CI -> People -> Purpose -> Background -> Scope -> Motivation ->
  ChunkDB -> SystemMeta
mkSystemMeta = SystemMeta
