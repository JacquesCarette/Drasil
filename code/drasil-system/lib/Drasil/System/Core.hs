{-# LANGUAGE TemplateHaskell #-}
module Drasil.System.Core where

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

-- | Enumeration of /kinds/ of 'System's we can encode.
data SystemKind =
    Specification
  | RunnableSoftware
  | Notebook
  | Website

data SystemMeta = SystemMeta
  { _sysName    :: CI -- FIXME: This should not be a CI.
  , _kind       :: SystemKind
  , _authors    :: People
  , _purpose    :: Purpose
  , _background :: Background
  , _scope      :: Scope
  , _motivation :: Motivation
  , _systemdb   :: ChunkDB
  }

makeClassy ''SystemMeta
