-- | Short names are used for displaying references.
module Drasil.Language.ShortName where

import Language.Drasil.Sentence

-- * Type

-- | Used for holding the short form of a name (as a 'Sentence' with a wrapper).
newtype ShortName = ShortNm Sentence

-- * Class

-- | A 'ShortName' is the text to be displayed for a link.
--
--   Used for referencing within a document that can include symbols and whatnot if required.
--   Visible in the typeset documents (pdf).
class HasShortName  s where
  shortname :: s -> ShortName

-- * Functions

-- | Pulls the short form (as a 'Sentence') out of a 'ShortName'.
getSentSN :: ShortName -> Sentence
getSentSN (ShortNm s) = s

-- | Smart constructor for making a 'Sentence' into a 'ShortName'.
shortname' :: Sentence -> ShortName
shortname' = ShortNm
