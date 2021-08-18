-- | Unique Identifier used across Drasil.
module Language.Drasil.UID (UID(..)) where

-- | A @UID@ is a 'unique identifier' for things that we will put into our database
-- of information. We use a newtype wrapper to make sure we are only using
-- 'UID's where desired.
newtype UID = UID {uidToStr :: String} deriving (Eq, Ord)

instance Show UID where
  show (UID u) = u