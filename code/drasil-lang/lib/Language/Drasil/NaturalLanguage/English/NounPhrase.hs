module Language.Drasil.NaturalLanguage.English.NounPhrase (
  -- * Types
  NounPhrase(..), NP,
  -- * Phrase Accessors
  atStartNP, atStartNP', titleizeNP, titleizeNP',
  -- * Constructors
  -- ** Common Noun Constructors
  cn, cn', cn'', cn''', cnICES, cnIES, cnIP, cnIS, cnIrr, cnUM,
  -- ** Proper Noun Constructors
  pn, pn', pn'', pn''', pnIrr,
  -- ** Noun Phrase Constructors
  nounPhrase, nounPhrase', nounPhrase'', nounPhraseSP, nounPhraseSent,
  -- * Combinators
  compoundPhrase,
  compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1,
  surroundNPStruct,
  -- * Re-exported Types
  CapitalizationRuleG(..), CapitalizationRule, PluralRule(..), NPStruct,
  PluralForm, NPG, NPStructG,
  -- * Re-exported Smart Constructors
  npS, npP, (.-.), (.+.)
  ) where

import Drasil.NaturalLanguage.English.NounPhrase -- uses whole module
import Language.Drasil.Symbol (Symbol)

-- | Synonym for 'NPStructG' filled with a 'Symbol' the version
-- used outside this file.
type NPStruct = NPStructG Symbol

-- | 'PluralFormG' filled with a 'Symbol' the version used outside this file.
type PluralForm = PluralFormG Symbol

-- | 'CapitalizationRuleG' filled with a 'Symbol' the version used outside this file.
type CapitalizationRule = CapitalizationRuleG Symbol

-- | 'NPG' filled with a 'Symbol' the version used outside this file.
type NP = NPG Symbol
