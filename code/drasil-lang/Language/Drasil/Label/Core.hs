{-# Language TemplateHaskell #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses)

import Language.Drasil.UID (UID)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname), HasRefAddress(getRefAdd))
import Language.Drasil.Label.Type (LblType)

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID :: UID --internal, unique
  , _lblType  :: LblType
  , _sn       :: ShortName
  }
makeLenses ''Label

instance HasUID       Label where uid       = uniqueID
instance HasShortName Label where shortname = sn
instance HasRefAddress Label where getRefAdd = lblType
