module Language.Drasil.ShortName where

-- | Used for holding the short form of a name (as a String with a wrapper).
newtype ShortName = ShortNm String

-- | Pulls the short form (as a 'String') out of a 'ShortName'.
getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s

-- | Smart constructor for making a 'String' into a 'ShortName'.
shortname' :: String -> ShortName
shortname' = ShortNm
