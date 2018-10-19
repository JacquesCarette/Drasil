{-# Language TemplateHaskell #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses)

import Language.Drasil.UID (UID)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname), HasRefAddress(getRefAdd))
import Language.Drasil.Label.Type (LblType)
import Language.Drasil.RefTypes (RefType)

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID :: UID --internal, unique
  , _lblType  :: LblType
  , _sn       :: ShortName
  , rftype    :: RefType --FIXME: HACK; ONLY USED FOR DIRECTLY REFERENCING LABELS PROPERLY in Reference.hs (see #971)
  }
makeLenses ''Label

instance HasUID       Label where uid       = uniqueID
instance HasShortName Label where shortname = sn
instance HasRefAddress Label where getRefAdd = lblType
