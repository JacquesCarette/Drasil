module Language.Drasil.Label (Label, mkLabelRA, mkLabelRA',
 mkLabelRA'', mkEmptyLabel, getAdd, mkLabelRAReady,
 mkLabelRAAssump', mkLabelRAFig) where

import Language.Drasil.Label.Core
import Language.Drasil.Classes (HasUID(uid), HasRefAddress(getRefAdd))
import Data.Char (isAscii)
import Language.Drasil.Chunk.ShortName (shortname', ShortName, getStringSN, shortname)
import Language.Drasil.RefTypes (RefType(..))

import Control.Lens((^.))

instance HasUID        Label where uid       = uniqueID
instance HasRefAddress Label where getRefAdd = lblType

-- multiple mkLabel constructors for label creation
-- id     ==> unique ID for Drasil referencing (i.e. internal)
-- ref    ==> pure ASCII used for referencing
-- shortn ==> the shortname - what a person/import Language.Drasil.RefTypes (RefType(..))user will see (can include spaces, accents, etc. (unicode))
-- RefTypes are Sect by default
mkLabelRA :: String -> String -> String -> RefType -> Label
mkLabelRA i ref shortn rt = Lbl i (RefAdd $ ensureASCII ref) (shortname' shortn) rt

-- for when reference address and the display should be the different
mkLabelRA' :: String -> String -> Label
mkLabelRA' ref shortn = Lbl (ref ++ "Label") (RefAdd $ ensureASCII ref) 
  (shortname' shortn) Sect

-- for when reference address and the display should be the same
mkLabelRA'' :: String -> Label
mkLabelRA'' iAndRefAndshortn = Lbl (iAndRefAndshortn ++ "Label") 
  (RefAdd $ ensureASCII iAndRefAndshortn) (shortname' iAndRefAndshortn) Sect

mkLabelRAReady :: String -> ShortName -> RefType -> Label
mkLabelRAReady r s t = Lbl (r ++ "Label") (RefAdd r) s t

mkLabelRAAssump :: String -> String -> Label
mkLabelRAAssump r s = mkLabelRA (r ++ "Label") ("A:" ++ ensureASCII r) 
  ("A: " ++s) Assump

mkLabelRAAssump' :: String -> Label
mkLabelRAAssump' rs = mkLabelRAAssump rs rs

--label constructor for Graphs and Figures
mkLabelRAFig :: String -> Label
mkLabelRAFig x = mkLabelRA (x ++ "Label") ("Figure:" ++ ensureASCII x) x Fig

mkLabelML :: String -> String -> String -> Label
mkLabelML i ref shortn = Lbl i (MetaLink $ ensureASCII ref) (shortname' shortn) Sect

mkLabelURI :: String -> String -> String -> Label
mkLabelURI i ref shortn = Lbl i (URI $ ensureASCII ref) (shortname' shortn) Sect

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s

--FIXME: remove upon adding labels to all the data types
mkEmptyLabel :: Label
mkEmptyLabel = mkLabelRA'' "empty"