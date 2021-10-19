-- | Short names are used for displaying references.
module Language.Drasil.ShortName where

import Language.Drasil.Sentence

-- * Type

-- | Used for holding the short form of a name (as a 'Sentence' with a wrapper).
newtype ShortName = ShortNm Sentence

-- * Functions

-- | Pulls the short form (as a 'Sentence') out of a 'ShortName'.
getSentSN :: ShortName -> Sentence
getSentSN (ShortNm s) = s

-- | Smart constructor for making a 'Sentence' into a 'ShortName'.
shortname' :: Sentence -> ShortName
shortname' = ShortNm
