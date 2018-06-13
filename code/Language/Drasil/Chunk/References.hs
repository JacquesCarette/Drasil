module Language.Drasil.Chunk.References where

import Language.Drasil.Spec (Sentence)

data Reference = SourceRef Sentence -- Source to reference for this knowledge chunk
                                    -- FIXME: Allow URLs/Citations here

type References = [Reference]