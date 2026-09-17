-- | Basic data types for noun phrases.
module Language.Drasil.NaturalLanguage.English.NounPhrase.Core (
  -- * Types
  CapitalizationRuleG(..),
  NPG(..),
  PluralFormG,
  PluralRule(..),
  NPStructG(SC, PC, (:-!:), (:+!:)),
  -- | smart constructors
  npS, npP, (.-.), (.+.)
) where

import Drasil.Database (HasChunkRefs(..))

-- | Essentially a subset of 'Sentence' that contains only the parts
-- that make sense for a NounPhrase
-- use smart constructor here instead 's', 'p', '.-.', '.+.'
data NPStructG a =
    SC String
  | NPStructG a :-!: NPStructG a -- no space
  | NPStructG a :+!: NPStructG a -- a space
  | PC a

-- | smart constructor for a literal string
npS :: String -> NPStructG a
npS = SC

-- | smart constructor for a literal string
npP :: a -> NPStructG a
npP = PC

-- | smart constructor: join two 'NPStructG's with a space in between
(.+.) :: NPStructG a -> NPStructG a -> NPStructG a
(.+.) = (:+!:)

-- | smart constructor: join two 'NPStructG's with no space in between
(.-.) :: NPStructG a -> NPStructG a -> NPStructG a
(.-.) = (:-!:)

-- | Synonym for 'NPStructG' typically used for plural forms.
type PluralFormG a = NPStructG a

-- | Capitalization rules.
data CapitalizationRuleG a =
    CapFirst -- ^ Capitalize the first letter of the first word only.
  | CapWords -- ^ Capitalize the first letter of each word.
  | Replace (NPStructG a) -- ^ Replace the noun phrase with the given
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
data NPG a =
    ProperNoun String PluralRule -- ^ Stores a proper noun and its pluralization.
  | CommonNoun String PluralRule (CapitalizationRuleG a) -- ^ Stores a common noun and its pluralization.
  | Phrase     (NPStructG a) (PluralFormG a) (CapitalizationRuleG a) (CapitalizationRuleG a) -- ^ Stores noun phrase and its pluralization.

-- | Gather the chunk references mentioned within an 'NP'.
instance HasChunkRefs (NPG a) where
    -- NPStructG only contains Strings, so it cannot embed UID refs.
    chunkRefs _ = mempty
    {-# INLINABLE chunkRefs #-}
