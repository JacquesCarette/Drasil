module Language.Drasil.Label (Label) where

import Language.Drasil.Label.Core
import Language.Drasil.Classes (HasUID(uid))
import Data.Char (isAscii)

instance HasUID Label where uid = uniqueID

-- multiple mkLabel constructors for label creation

mkLabelRA :: String -> String -> Label
mkLabelRA id ref = Lbl id (RefAdd $ ensureASCII ref)

mkLabelML :: String -> String -> Label
mkLabelML id ref = Lbl id (MetaLink $ ensureASCII ref)

mkLabelURI :: String -> String -> Label
mkLabelURI id ref = Lbl id (URI $ ensureASCII ref)

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s