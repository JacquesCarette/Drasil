-- | Unique Identifier used across Drasil.
module Language.Drasil.UID (UID(..), toUID) where

-- | A @UID@ is a 'unique identifier' for things that we will put into our database
-- of information. We use a newtype wrapper to make sure we are only using
-- 'UID's where desired.
newtype UID = UID {uidToStr :: String} deriving (Eq, Show, Ord)

-- | Smart constructor for making a 'UID' from a 'String'.
toUID :: String -> UID
toUID s
  | '►' `elem` s = error "► not allowed in UID"
  | null s       = error "UID must be non-zero length"
  | otherwise    = UID s