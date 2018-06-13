{-# Language TemplateHaskell #-}
module Language.Drasil.Label (Label) where

import Control.Lens (makeLenses, Lens')
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))
import Data.Char (isAscii)

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
mkLabelRA id ref = Lbl id (RefAdd $ ensureASCII ref)

mkLabelML :: String -> String -> Label
mkLabelML id ref = Lbl id (MetaLink $ ensureASCII ref)

mkLabelURI :: String -> String -> Label
mkLabelURI id ref = Lbl id (URI $ ensureASCII ref)

-- isLabel and HasLabel classes

-- | For those things which "have a label"
class HasLabel c where
  getLabel :: Lens' c Label

-- IsLabel is associated with String rendering
class (HasLabel u, HasUID u) => IsLabel u where

-- helpers
ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s