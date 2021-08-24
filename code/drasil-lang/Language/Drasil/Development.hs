-- | Developing the expression language in Drasil. Re-export many things to simplify external use.
module Language.Drasil.Development (
  -- * NounPhrase
    NounPhrase(phraseNP, pluralNP)
  -- Sentence.Extract
  , sdep, lnames, lnames'
  -- * UID
  , uid, showUID
  ) where

import Language.Drasil.NounPhrase (NounPhrase(phraseNP,pluralNP))
import Language.Drasil.Sentence.Extract (sdep, lnames, lnames')
import Language.Drasil.UID.Core (uid, showUID)
