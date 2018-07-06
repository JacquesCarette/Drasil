module Language.Drasil.Sentence.Math (ch) where

import Control.Lens ((^.))
import Language.Drasil.Classes (HasUID(uid), HasSymbol)
import Language.Drasil.Spec(Sentence(..))

ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = Ch (x ^. uid)
