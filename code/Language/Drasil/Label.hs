module Language.Drasil.Label (Label, mkLabelRA) where

import Language.Drasil.Label.Core
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd))
import Data.Char (isAscii)
import Language.Drasil.Chunk.ShortName (shortname')

instance HasUID        Label where uid       = uniqueID
instance HasRefAddress Label where getRefAdd = lblType

-- multiple mkLabel constructors for label creation
-- id     ==> unique ID for Drasil referencing (i.e. internal)
-- ref    ==> pure ASCII used for referencing
-- shortn ==> the shortname - what a person/user will see (can include spaces, accents, etc. (unicode))
mkLabelRA :: String -> String -> String -> Label
mkLabelRA id ref shortn = Lbl id (RefAdd $ ensureASCII ref) (shortname' shortn)

mkLabelML :: String -> String -> String -> Label
mkLabelML id ref shortn = Lbl id (MetaLink $ ensureASCII ref) (shortname' shortn)

mkLabelURI :: String -> String -> String -> Label
mkLabelURI id ref shortn = Lbl id (URI $ ensureASCII ref) (shortname' shortn)

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s