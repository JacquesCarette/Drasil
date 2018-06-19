module Language.Drasil.Label (Label, mkLabelRA) where

import Language.Drasil.Label.Core
import Language.Drasil.Classes (HasUID(uid))
import Data.Char (isAscii)
import Language.Drasil.Chunk.ShortName (shortname')

instance HasUID Label where uid = uniqueID

-- multiple mkLabel constructors for label creation
-- id     ==> unique ID for Drasil referencing (i.e. internal)
-- ref    ==> pure ASCII used for referencing
-- shortn ==> the shortname - what a person/user will see (can include spaces, accents, etc. (unicode))
mkLabelRA :: String -> String -> Maybe String -> Label
mkLabelRA id ref (Just shortn) = Lbl id (RefAdd $ ensureASCII ref) (Just (shortname' shortn))
mkLabelRA id ref Nothing       = Lbl id (RefAdd $ ensureASCII ref) Nothing

mkLabelML :: String -> String -> Maybe String -> Label
mkLabelML id ref (Just shortn) = Lbl id (MetaLink $ ensureASCII ref) (Just (shortname' shortn))
mkLabelML id ref Nothing       = Lbl id (MetaLink $ ensureASCII ref) Nothing

mkLabelURI :: String -> String -> Maybe String -> Label
mkLabelURI id ref (Just shortn) = Lbl id (URI $ ensureASCII ref) (Just (shortname' shortn))
mkLabelURI id ref Nothing       = Lbl id (URI $ ensureASCII ref) Nothing

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s