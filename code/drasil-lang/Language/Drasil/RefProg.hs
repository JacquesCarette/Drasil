{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg (Reference(Reference)) where
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Label.Type (LblType, getAdd)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses)

data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType     -- the main string of the reference address
  ,  sn :: ShortName } -- the human-readable short name
makeLenses ''Reference

instance HasUID        Reference where uid = ui
instance HasRefAddress Reference where getRefAdd = getAdd . ra
instance HasShortName  Reference where shortname = sn
