{-# Language TemplateHaskell #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses, view)
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname))

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID  :: UID --internal, unique
  , lblType    :: LblType
  , _sn        :: ShortName
  }
makeLenses ''Label

instance HasShortName Label where shortname = view sn
