module Language.Drasil.Chunk.Attribute ( getShortName, snToSentence) where

import Control.Lens ((^.))

import Language.Drasil.ShortName (ShortName, getStringSN)

import Language.Drasil.Classes (HasShortName(shortname))
import Language.Drasil.Sentence (Sentence(S))

--------------------------------------------------------------------------------

getShortName :: HasShortName c => c -> Sentence
getShortName c = snToSentence $ c ^. shortname

snToSentence :: ShortName -> Sentence
snToSentence = S . getStringSN
