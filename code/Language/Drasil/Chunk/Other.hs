module Language.Drasil.Chunk.Other where

import Language.Drasil.Chunk.Wrapper

-- | Assumption chunks are just synonyms for 'NWrappers' (They contain a 'NamedChunk')
type AssumpChunk = NWrapper

-- | Unlikely change chunks are just synonyms for 'NWrappers' (They contain a 'NamedChunk')
type UCChunk = NWrapper