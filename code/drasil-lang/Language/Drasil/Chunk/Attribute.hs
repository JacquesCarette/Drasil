module Language.Drasil.Chunk.Attribute ( getShortName ) where

import Control.Lens ((^.))

import Language.Drasil.ShortName (getStringSN)

import Language.Drasil.Classes (HasShortName(shortname))
import Language.Drasil.Sentence (Sentence(S))

--------------------------------------------------------------------------------

getShortName :: HasShortName c => c -> Sentence
getShortName c = S . getStringSN $ c ^. shortname
