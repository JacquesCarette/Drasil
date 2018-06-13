{-# Language TemplateHaskell #-}
module Language.Drasil.Label (Label) where

import Control.Lens (makeLenses)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))

-- import reference address from Language.Drasil.References?
data LblType = RefAdd | MetaLink | URI

data Label = Lbl 
  { _uniqueID  :: UID
  , lblType    :: LblType 
  }
makeLenses ''Label
  
instance HasUID Label where uid = uniqueID