-- | Developing the expression language in Drasil. Re-export many things to simplify external use.
module Language.Drasil.Development (
  -- * Development.Sentence
  toSent
  -- * NounPhrase
  , NounPhrase(phraseNP, pluralNP)
  -- * NounPhrase.Core
  , NPStruct(S, (:+:), (:-:), P)
  -- Sentence.Extract
  , sdep, shortdep, lnames, lnames'
  -- * UID
  , showUID
) where

import Language.Drasil.Development.Sentence (toSent)
import Language.Drasil.NounPhrase (NounPhrase(..))
import Language.Drasil.NounPhrase.Core (NPStruct(..))
import Language.Drasil.Sentence.Extract (sdep, shortdep, lnames, lnames')
import Drasil.Database (showUID)
