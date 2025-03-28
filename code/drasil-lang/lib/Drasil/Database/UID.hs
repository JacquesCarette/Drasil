{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Unique Identifier used across Drasil.
module Drasil.Database.UID (
    UID
  , HasUID(uid)
  , mkUid, nsUid, (+++), (+++.), (+++!)
  , showUID
) where

import Data.Aeson
import Data.Aeson.Types
import Data.List (intercalate)
import Data.Text (pack)
import GHC.Generics

import Control.Lens (Getter, makeLenses, (^.), view, over)

-- | The most basic item: having a unique identifier key, here a UID.
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use.
  uid :: Getter c UID

-- | A @UID@ is a 'unique identifier' for things that we will put into our
-- database of information. We use a newtype wrapper to make sure we are only
-- using 'UID's where desired.
data UID = UID { _namespace :: [String], _baseName :: String }
  deriving (Eq, Ord, Generic)

makeLenses ''UID

instance ToJSON UID where
  toJSON = toJSON . show

instance ToJSONKey UID where
  toJSONKey = toJSONKeyText (pack . show)

instance Show UID where
  show u = intercalate ":" $ u ^. namespace ++ [u ^. baseName]

-- | Smart constructor for 'UID's from raw 'String's.
mkUid :: String -> UID
mkUid s = UID { _namespace = [], _baseName = s }

-- | Nest a 'UID' under a namespace.
nsUid :: String -> UID -> UID
nsUid ns = over namespace (++ [ns])

-- | Append a suffix to a thing with a 'UID' and get the resulting 'UID'.
(+++) :: HasUID a => a -> String -> UID
(+++) a = (+++.) (a ^. uid)

-- | Append a suffix to a 'UID'.
(+++.) :: UID -> String -> UID
a +++. suff
  | null suff       = error $ "Suffix must be non-zero length for UID " ++ show a
  | otherwise       = over baseName (++ suff) a

-- | Merge the 'UID's of two chunks through simple concatenation.
(+++!) :: (HasUID a, HasUID b) => a -> b -> UID
a +++! b
  | s ^. namespace /= t ^. namespace = error $ show s ++ " and " ++ show t ++ " are not in the same namespace"
  | null (s ^. baseName) || null (t ^. baseName) = error $ show s ++ " and " ++ show t ++ " UIDs must be non-zero length"
  | otherwise = s +++. (t ^. baseName)
  where
    s = a ^. uid
    t = b ^. uid

-- | Get something's 'UID' as a 'String'.
showUID :: HasUID a => a -> String
showUID = show . view uid
