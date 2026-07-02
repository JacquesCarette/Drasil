{-# LANGUAGE FunctionalDependencies #-}
module Drasil.System.Render (
  ToFiles(..)
) where

import Control.Lens ((^.))

import Drasil.FileHandling (FileLayout, directory, ps)

import Drasil.System.Core (HasSystemMeta(..))

-- | The goal of our systems is to be abstractions about human-readable software
-- artifacts. An instance of this typeclass ('ToFiles') defines a software
-- generator that explains how said abstractions can be made /fully concrete/
-- (i.e., made into concrete software artifacts).
class HasSystemMeta sys => ToFiles sys opts | opts -> sys where
  -- FIXME: Should we try to name renderers? Should we associate them with
  -- chunks so we can refer to them in code as well? Perhaps this will give us
  -- usage statistics, if we have a fine-grained enough `Render`-reliant scheme
  -- and were polymorphic about the output type. Going monadic here can help
  -- give us usage statistics on what things went on in the generation pipeline.
  toFiles ::
    -- | The system.
    sys ->
    -- | The generation options.
    opts ->
    -- | The final, rendered software artifacts.
    [FileLayout]
