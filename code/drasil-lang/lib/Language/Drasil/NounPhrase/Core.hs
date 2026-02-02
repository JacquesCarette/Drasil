-- | Basic data types for noun phrases.
module Language.Drasil.NounPhrase.Core (
  -- * Types
  CapitalizationRule(..), NP(..),
  PluralForm, PluralRule(..),
  NPStruct(S,(:-:),(:+:),P)
) where

import Drasil.Database (HasChunkRefs(..))

import Language.Drasil.Symbol (Symbol)

-- | Essentially a subset of 'Sentence' that contains only the parts
-- that make sense for a NounPhrase
data NPStruct =
    S String
  | NPStruct :-: NPStruct -- no space
  | NPStruct :+: NPStruct -- a space
  | P Symbol

-- | Synonym for 'NPStruct' typically used for plural forms.
type PluralForm = NPStruct

-- | Capitalization rules.
data CapitalizationRule =
    CapFirst -- ^ Capitalize the first letter of the first word only.
  | CapWords -- ^ Capitalize the first letter of each word.
  | Replace NPStruct -- ^ Replace the noun phrase with the given
                     -- 'NPStruct'. Used for custom capitalization.
  | CapNothing    -- some parts of speech don't capitalize at all but still a full phrase

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
data NP =
    ProperNoun String PluralRule -- ^ Stores a proper noun and its pluralization.
  | CommonNoun String PluralRule CapitalizationRule -- ^ Stores a common noun and its pluralization.
  | Phrase     NPStruct PluralForm CapitalizationRule CapitalizationRule -- ^ Stores noun phrase and its pluralization.
  --Phrase plurals can get very odd, so it seems best (for now) to encode
  --them directly. FIXME: If the singular/plural phrase has special (replace)
  --capitalization, one of the two cannot be capitalized right now.
  --The two capitalization rules are for sentenceCase / titleCase respectively

-- | Gather the chunk references mentioned within an 'NP'.
instance HasChunkRefs NP where
    -- NPStruct only contains Strings and Symbols, so it cannot embed UID refs.
    chunkRefs _ = mempty
    {-# INLINABLE chunkRefs #-}
