{-# Language TemplateHaskell #-}
module Language.Drasil.Label (Label) where

import Control.Lens (makeLenses)
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))

-- import reference address from Language.Drasil.References?
data LblType = RefAdd String | MetaLink String | URI String

-- Used for referencing; has to be pure ASCII
data Label = Lbl
  { _uniqueID  :: UID --internal, unique
  , lblType    :: LblType 
  }
makeLenses ''Label
  
instance HasUID Label where uid = uniqueID

-- multiple mkLabel constructors for label creation

mkLabelRA :: String -> String -> Label
mkLabelRA id ref = Lbl id (RefAdd ref)

mkLabelML :: String -> String -> Label
mkLabelML id ref = Lbl id (MetaLink ref)

mkLabelURI :: String -> String -> Label
mkLabelURI id ref = Lbl id (URI ref)