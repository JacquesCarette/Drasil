{-# Language TemplateHaskell #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses)
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.ShortName (ShortName)

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID  :: UID --internal, unique
  , lblType    :: LblType
  , sn         :: Maybe ShortName
  }
makeLenses ''Label