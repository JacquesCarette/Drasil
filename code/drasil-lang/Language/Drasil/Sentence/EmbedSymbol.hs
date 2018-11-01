module Language.Drasil.Sentence.EmbedSymbol (ch) where

import Control.Lens ((^.))
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Sentence (Sentence(Ch))

ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = Ch (x ^. uid)
