{-# Language TemplateHaskell #-}
module Language.Drasil.Label where

import Control.Lens (makeLenses)
import Language.Drasil.UID (UID)

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID  :: UID --internal, unique
  , lblType    :: LblType 
  }
makeLenses ''Label