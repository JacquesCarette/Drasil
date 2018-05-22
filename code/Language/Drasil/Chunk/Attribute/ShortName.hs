module Language.Drasil.Chunk.Attribute.ShortName where

import Language.Drasil.Spec (Sentence)

data ShortNameD = ShortName Sentence

type ShortName = Sentence