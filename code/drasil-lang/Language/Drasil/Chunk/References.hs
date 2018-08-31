module Language.Drasil.Chunk.References where

import Language.Drasil.Spec (Sentence)

type Reference = Sentence -- Source to reference for this knowledge chunk
                          -- FIXME: Allow URLs/Citations here