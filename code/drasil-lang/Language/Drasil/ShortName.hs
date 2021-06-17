module Language.Drasil.ShortName where

import Language.Drasil.Sentence

-- | Used for holding the short form of a name (as a String with a wrapper).
newtype ShortName = ShortNm Sentence

-- | Pulls the short form (as a 'String') out of a 'ShortName'.
getSentSN :: ShortName -> Sentence
getSentSN (ShortNm s) = s

-- | Smart constructor for making a 'String' into a 'ShortName'.
shortname' :: Sentence -> ShortName
shortname' = ShortNm
