-- | Noun phrases are used to hold terms with knowledge of proper capitalization and pluralization.
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Drasil.NounPhrase.Core (
  module Language.Drasil.NounPhrase.Types
) where

import Drasil.Database (HasChunkRefs(..))

import Language.Drasil.NounPhrase.Types

-- | Gather the chunk references mentioned within an 'NP'.
instance HasChunkRefs NP where
    -- NPStruct only contains Strings and Symbols, so it cannot embed UID refs.
    chunkRefs _ = mempty
    {-# INLINABLE chunkRefs #-}
