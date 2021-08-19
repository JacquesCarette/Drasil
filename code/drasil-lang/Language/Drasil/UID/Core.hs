module Language.Drasil.UID.Core (uid, (+++), (+++.), (+++!), showUID) where

import Language.Drasil.UID
import qualified Language.Drasil.Classes.Core as C (HasUID(uid))
import Control.Lens (view, (^.))


-- | Smart constructor for making a 'UID' from a 'String'.
uid :: String -> UID
uid = UID
  -- '►' `elem` s = error $ "► not allowed in UID " ++ show s -- FIXME: Need to implement other constructors before we can use this.
  -- null s       = error "UID must be non-zero length" -- FIXME: See Drasil.DocumentLanguage.TraceabilityGraph (uses an empty UID)
  -- otherwise    = UID s

-- | For when we need to modify a UID. We first take the base chunk's UID and then append a suffix to it.
(+++) :: C.HasUID a => a -> String -> UID
a +++ suff
  | null suff       = error "Suffix must be non-zero length"
  | otherwise       = UID $ s ++ suff
  -- otherwise       = UID $ s ++ '►':suff --FIXME: Implement this properly.
    where UID s = a ^. C.uid

-- | For when we need to append something to a UID.
(+++.) :: UID -> String -> UID
a +++. suff
  | null suff       = error $ "Suffix must be non-zero length for UID " ++ show a
  | otherwise       = UID $ s ++ suff
    where UID s = a

(+++!) :: (C.HasUID a, C.HasUID b) => a -> b -> UID
a +++! b 
  | null (showUID a) || null (showUID b) = error $ showUID a ++ " and " ++ showUID b ++ " UIDs must be non-zero length"
  | otherwise = UID (showUID a ++ showUID b)

-- | Grabs the UID from something that has a UID and displays it as a String.
showUID :: C.HasUID a => a -> String
showUID = show . view C.uid