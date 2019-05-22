{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg (InfoType(..), Reference(Reference, ri), RefInfo(..)) where

import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Label.Type (LblType, getAdd)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses)

data InfoType = Equation | Figure | Page

data RefInfo = None | RI { typ :: InfoType, nums :: [Int] }
makeLenses ''RefInfo

data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType   -- the main string of the reference address
  ,  sn :: ShortName -- the human-readable short name
  ,  ri :: RefInfo }
makeLenses ''Reference

instance HasUID        Reference where uid = ui
instance HasRefAddress Reference where getRefAdd = getAdd . ra
instance HasShortName  Reference where shortname = sn
