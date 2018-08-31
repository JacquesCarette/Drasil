module Language.Drasil.Chunk.ShortName where

import Language.Drasil.UID (UID)
import Control.Lens (Lens')

data DeferredCtx =
  FromCC UID
  deriving Eq  --FIXME: HACK FOR Document/Extract to work. We don't necessarily want this. (By extension of ShortName)

data ShortName =
    ShortNm String
  | Concat ShortName ShortName
  | Deferred DeferredCtx
    deriving Eq  --FIXME: HACK FOR Document/Extract to work. We don't necessarily want this.

instance Monoid ShortName where
  mempty = shortname' ""
  mappend = Concat

getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s
getStringSN (Concat a b) = (getStringSN a) ++ getStringSN b
getStringSN (Deferred _) = error "Unable to get a string from a deferred ShortName"

class HasShortName  s where
  shortname :: Lens' s ShortName-- String; The text to be displayed for the link.
                            -- A short name used for referencing within a document that can 
                            -- include symbols and whatnot if required.
                            -- Visible in the typeset documents (pdf)

shortname' :: String -> ShortName
shortname' = ShortNm

resolveSN :: ShortName -> (DeferredCtx -> String) -> ShortName
resolveSN (Deferred u) f = shortname' $ f u
resolveSN (Concat a b) f = Concat (resolveSN a f) $ resolveSN b f
resolveSN s _ = s
