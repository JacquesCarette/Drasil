{-# Language TemplateHaskell #-}
-- | References that have extra information.
module Language.Drasil.DecoratedReference (
  -- * Type
  DecRef(..),
  -- * Class
  HasDecRef(..),
  -- * Constructors
  dRef, dRefInfo
) where

import Language.Drasil.Sentence (RefInfo(..))
import Language.Drasil.Reference (Reference, ref)
import Language.Drasil.Label.Type (HasRefAddress(..))
import Language.Drasil.ShortName (HasShortName(..))
import Drasil.Database.UID (HasUID(..))
import Control.Lens ((^.), makeLenses, Lens')

-- | For holding a 'Reference' that is decorated with extra information (ex. page numbers, equation sources, etc.).
data DecRef = DR {
  _rf     :: Reference,
  refInfo :: RefInfo
}
makeLenses ''DecRef

-- | A class that contains a list of decorated references ('DecRef's).
class HasDecRef c where
  -- | Provides a 'Lens' to the 'DecRef's.
  getDecRefs :: Lens' c [DecRef]

-- | Equal if 'UID's are equal.
instance Eq            DecRef where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the 'UID' of a 'Reference'.
instance HasUID        DecRef where uid = rf . uid
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress DecRef where getRefAdd (DR r _) = getRefAdd r
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  DecRef where shortname (DR r _) = shortname r

-- | For creating a decorated reference ('DecRef') with extra reference information ('RefInfo').
dRefInfo :: (HasUID r, HasRefAddress r, HasShortName r) => r -> RefInfo -> DecRef
dRefInfo r = DR (ref r)

-- | Same as 'ref', but for 'DecRef' instead of 'Reference'.
dRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> DecRef
dRef r = dRefInfo r None
