module Language.Drasil.NounPhrase.Core (CapitalizationRule(..), NP(..),
  PluralForm, PluralRule(..)) where

import Language.Drasil.Sentence (Sentence)

type PluralForm = Sentence  -- These might change.

-- | Capitalization rules.
data CapitalizationRule = 
    CapFirst -- ^ Capitalize the first letter of the first word only.
  | CapWords -- ^ Capitalize the first letter of each word.
  | Replace Sentence -- ^ Replace the noun phrase with the
                                           -- given Sentence. Used for custom
                                           -- capitalization.

-- | Pluralization rules.
data PluralRule = 
    AddS -- ^ Add "s" to the end of the noun phrase.
  | AddE -- ^ Add "e" to the end of the noun phrase.
  | AddES -- ^ Add "es" to the end of the noun phrase.
  | SelfPlur -- ^ The noun phrase is already plural.
  | IrregPlur (String -> String) -- ^ Apply the given function to
                                               -- the noun phrase to get the plural.

data NP =
    ProperNoun String PluralRule
  | CommonNoun String PluralRule CapitalizationRule
  | Phrase     Sentence PluralForm CapitalizationRule CapitalizationRule
  --Phrase plurals can get very odd, so it seems best (for now) to encode
  --them directly. FIXME: If the singular/plural phrase has special (replace)
  --capitalization, one of the two cannot be capitalized right now.
  --The two capitalization rules are for sentenceCase / titleCase respectively

