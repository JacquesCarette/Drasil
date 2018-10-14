module Language.Drasil.ShortName(
  ShortName, getStringSN, resolveSN, shortname', concatSN, defer) where

import Language.Drasil.UID (UID)

data ShortName =
    ShortNm String
  | Concat ShortName ShortName
  | Deferred UID
    deriving Eq  --FIXME: HACK FOR Document/Extract to work. We don't necessarily want this.

instance Monoid ShortName where
  mempty = shortname' ""
  mappend = Concat

getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s
getStringSN (Concat a b) = (getStringSN a) ++ getStringSN b
getStringSN (Deferred _) = error "Unable to get a string from a deferred ShortName"

shortname' :: String -> ShortName
shortname' = ShortNm

concatSN :: ShortName -> ShortName -> ShortName
concatSN = Concat

defer :: UID -> ShortName
defer = Deferred

resolveSN :: ShortName -> (UID -> String) -> ShortName
resolveSN (Deferred u) f = shortname' $ f u
resolveSN (Concat a b) f = Concat (resolveSN a f) $ resolveSN b f
resolveSN s _ = s
