{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Drasil.System.Core (
  Purpose, Background, Scope, Motivation,
  SystemMeta,
  HasSystemMeta(..),
  mkSystemMeta,
) where

import Control.Lens ((^.), makeClassy)

import Drasil.Database (ChunkDB, HasUID(..), HasChunkRefs(..))
import Language.Drasil (Sentence, People, CI, NamedIdea(..), Idea(..), CommonIdea(..))

import Drasil.System.ProjectName (ProjectName, title, abbreviation)

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

instance HasUID SystemMeta where
  uid = projName . uid

instance HasChunkRefs SystemMeta where
  chunkRefs x = chunkRefs (x ^. projName)

instance NamedIdea SystemMeta where
  term = projName . title

instance Idea SystemMeta where
  getA x = Just (x ^. projName . abbreviation)

instance CommonIdea SystemMeta where
  abrv x = x ^. projName . abbreviation

mkSystemMeta :: ProjectName -> CI -> People -> Purpose -> Background -> Scope ->
  Motivation -> ChunkDB -> SystemMeta
mkSystemMeta = SystemMeta
