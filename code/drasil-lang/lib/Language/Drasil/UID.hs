{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Unique Identifier used across Drasil.
module Language.Drasil.UID (
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

-- | A @UID@ is a 'unique identifier' for things that we will put into our database
-- of information. We use a newtype wrapper to make sure we are only using
-- 'UID's where desired.
data UID = UID { _namespace :: [String], _baseName :: String }
  deriving (Eq, Ord, Generic) --, ToJSONKey)

makeLenses ''UID

fullName :: UID -> [String]
fullName u = u ^. namespace ++ [u ^. baseName]

instance ToJSON UID where
  toJSON = toJSON . fullName

instance ToJSONKey UID where
  toJSONKey = toJSONKeyText (pack . show)

instance Show UID where
  show = intercalate ":" . fullName

-- | Smart constructor for making a 'UID' from a 'String'.
mkUid :: String -> UID
mkUid s = UID { _namespace = [], _baseName = s }
  -- '►' `elem` s = error $ "► not allowed in UID " ++ show s -- FIXME: Need to implement other constructors before we can use this.
  -- null s       = error "UID must be non-zero length" -- FIXME: See Drasil.DocumentLanguage.TraceabilityGraph (uses an empty UID)
  -- otherwise    = UID s

-- | Nest UID under a namespace
nsUid :: String -> UID -> UID
nsUid ns = over namespace (ns:)

-- | For when we need to modify a UID. We first take the base chunk's UID and then append a suffix to it.
(+++) :: HasUID a => a -> String -> UID
a +++ suff
  | null suff       = error "Suffix must be non-zero length"
  | otherwise       = (a ^. uid) +++. suff
  -- otherwise       = UID $ s ++ '►':suff --FIXME: Implement this properly.
  --   where UID s = a ^. uid

-- | For when we need to append something to a UID.
(+++.) :: UID -> String -> UID
a +++. suff
  | null suff       = error $ "Suffix must be non-zero length for UID " ++ show a
  | otherwise       = over baseName (++ suff) a

(+++!) :: (HasUID a, HasUID b) => a -> b -> UID
a +++! b
  | s ^. namespace /= t ^. namespace = error $ show s ++ " and " ++ show t ++ " are not in the same namespace"
  | null (s ^. baseName) || null (t ^. baseName) = error $ show s ++ " and " ++ show t ++ " UIDs must be non-zero length"
  | otherwise = s +++. (t ^. baseName)
  where
    s = a ^. uid
    t = b ^. uid

-- | Grabs the UID from something that has a UID and displays it as a String.
showUID :: HasUID a => a -> String
showUID = show . view uid
