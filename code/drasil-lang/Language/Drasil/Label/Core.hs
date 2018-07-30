{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Language.Drasil.Label.Core where

import Control.Lens (makeLenses)
import Language.Drasil.UID (UID)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname))

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID  :: UID --internal, unique
  , _lblType    :: LblType
  , _sn        :: ShortName
  }
makeLenses ''Label

instance HasShortName Label where shortname = sn

getAdd :: LblType -> String
getAdd (RefAdd s)   = s
getAdd (MetaLink s) = s
getAdd (URI s)      = s