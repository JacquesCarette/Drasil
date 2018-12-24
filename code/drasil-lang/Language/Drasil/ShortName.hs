module Language.Drasil.ShortName(
  ShortName, getStringSN, shortname', concatSN) where

data ShortName =
    ShortNm String
  | Concat ShortName ShortName
    deriving Eq  --FIXME: HACK FOR Document/Extract to work. We don't necessarily want this.

instance Monoid ShortName where
  mempty = shortname' ""
  mappend = Concat

getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s
getStringSN (Concat a b) = (getStringSN a) ++ getStringSN b

shortname' :: String -> ShortName
shortname' = ShortNm

concatSN :: ShortName -> ShortName -> ShortName
concatSN = Concat
