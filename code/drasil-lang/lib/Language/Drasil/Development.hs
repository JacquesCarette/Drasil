-- | Developing the expression language in Drasil. Re-export many things to simplify external use.
module Language.Drasil.Development (
  -- * NounPhrase
    NounPhrase(phraseNP, pluralNP)
  -- Sentence.Extract
  , sdep, shortdep, lnames, lnames'
  -- * UID
  , showUID
) where

import Language.Drasil.NounPhrase (NounPhrase(..))
import Language.Drasil.Sentence.Extract (sdep, shortdep, lnames, lnames')
import Drasil.Database.UID (showUID)
