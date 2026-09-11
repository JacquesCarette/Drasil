-- | Basic data types for noun phrases.
module Language.Drasil.NaturalLanguage.English.NounPhrase.Core (
  -- * Types
  CapitalizationRuleG(..), CapitalizationRule,
  NPG(..), NP,
  PluralFormG, PluralForm,
  PluralRule(..),
  NPStructG(S,(:-:),(:+:),P), NPStruct
) where

import Drasil.Database (HasChunkRefs(..))

import Language.Drasil.Symbol (Symbol)

-- | Essentially a subset of 'Sentence' that contains only the parts
-- that make sense for a NounPhrase
data NPStructG a =
    S String
  | NPStructG a :-: NPStructG a -- no space
  | NPStructG a :+: NPStructG a -- a space
  | P a

-- | Synonym for 'NPStructG' filled with a 'Symbol' the version
-- used outside the file.
type NPStruct = NPStructG Symbol

-- | Synonym for 'NPStructG' typically used for plural forms.
type PluralFormG a = NPStructG a
-- | 'PluralFormG' filled with a 'Symbol' the version used outside the file.
type PluralForm = PluralFormG Symbol

-- | Capitalization rules.
data CapitalizationRuleG a =
    CapFirst -- ^ Capitalize the first letter of the first word only.
  | CapWords -- ^ Capitalize the first letter of each word.
  | Replace (NPStructG a) -- ^ Replace the noun phrase with the given
                     -- 'NPStruct'. Used for custom capitalization.
  | CapNothing    -- some parts of speech don't capitalize at all but still a full phrase

-- | 'CapitalizationRuleG' filled with a 'Symbol' the version used outside the file.
type CapitalizationRule = CapitalizationRuleG Symbol

-- | Pluralization rules.
data PluralRule =
    AddS -- ^ Add "s" to the end of the noun phrase.
  | AddE -- ^ Add "e" to the end of the noun phrase.
  | AddES -- ^ Add "es" to the end of the noun phrase.
  | SelfPlur -- ^ The noun phrase is already plural.
  | IrregPlur (String -> String) -- ^ Apply the given function to
                                               -- the noun phrase to get the plural.

-- | For nouns and 'NounPhrase's. May be constructed from a
-- proper noun, common noun, or phrase ('Sentence') and their
-- respective pluralization and capitalization rules.
data NPG a =
    ProperNoun String PluralRule -- ^ Stores a proper noun and its pluralization.
  | CommonNoun String PluralRule (CapitalizationRuleG a) -- ^ Stores a common noun and its pluralization.
  | Phrase     (NPStructG a) (PluralFormG a) (CapitalizationRuleG a) (CapitalizationRuleG a) -- ^ Stores noun phrase and its pluralization.
  --Phrase plurals can get very odd, so it seems best (for now) to encode
  --them directly. FIXME: If the singular/plural phrase has special (replace)
  --capitalization, one of the two cannot be capitalized right now.
  --The two capitalization rules are for sentenceCase / titleCase respectively

-- | 'NPG' filled with a 'Symbol' the version used outside the file.
type NP = NPG Symbol

-- | Gather the chunk references mentioned within an 'NP'.
instance HasChunkRefs (NPG a) where
    -- NPStructG only contains Strings, so it cannot embed UID refs.
    chunkRefs _ = mempty
    {-# INLINABLE chunkRefs #-}
