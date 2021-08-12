{- re-export many things to simplify external use -}
module Language.Drasil.Development (
  -- NounPhrase
    NounPhrase(phraseNP, pluralNP)
  -- Sentence.Extract
  , sdep, lnames, lnames'
  ) where

import Language.Drasil.NounPhrase (NounPhrase(phraseNP,pluralNP))
import Language.Drasil.Sentence.Extract (sdep, lnames, lnames')
