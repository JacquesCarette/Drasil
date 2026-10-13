{-# LANGUAGE FunctionalDependencies #-}

module Drasil.System.Transformations
  ( ToFiles (..),
  )
where

import Drasil.FileHandling (FileLayout)
import Drasil.System.Core (HasSystemMeta(..))
import Drasil.System.ProjectName (HasProjectName(..))

-- | The goal of our systems is to be abstractions about human-readable software
-- artifacts. An instance of this typeclass ('ToFiles') defines a software
-- generator that explains how said abstractions can be made /fully concrete/
-- (i.e., made into concrete software artifacts).
class (HasSystemMeta sys, HasProjectName sys) => ToFiles sys opts | opts -> sys where
  toFiles ::
    -- | The system.
    sys ->
    -- | The generation options.
    opts ->
    -- | The final, rendered software artifacts.
    [FileLayout]

-- FIXME: Should we try to name our generators? Should we associate them with
-- chunks so we can refer to them in code as well? Perhaps this will give us
-- usage statistics, if we have a fine-grained enough `Render`-reliant scheme
-- and were polymorphic about the output type. Going monadic here can help give
-- us usage statistics on what things went on in the generation pipeline.
