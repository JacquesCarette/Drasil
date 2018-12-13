module Language.Drasil.Label (Label, mkLabelRA',
 mkLabelSame, mkURILabel, getAdd, mkLabelRAAssump', 
 mkLabelRAFig, mkLabelRASec, mkLabelRALst,
 getDefName) where

import Data.Char (isAscii)

import Language.Drasil.Label.Core
import Language.Drasil.Label.Type (LblType(..), getAdd)
import Language.Drasil.ShortName (shortname')
import Language.Drasil.RefTypes (RefType(..), DType(..))

-- multiple mkLabel constructors for label creation
-- id     ==> unique ID for Drasil referencing (i.e. internal)
-- ref    ==> pure ASCII used for referencing
-- shortn ==> the shortname - what a person/user will see (can include spaces, accents, etc. (unicode))
mkLabel :: String -> String -> String -> RefType -> Label
mkLabel lblUID ra sn' rtype = Lbl lblUID 
  (RefAdd $ concatMap repUnd $ getAcc rtype ++ ensureASCII ra)
  (shortname' sn')

mkURILabel :: String -> String -> String -> Label
mkURILabel lblUID ra sn' = Lbl lblUID 
  (URI $ ensureASCII ra)
  (shortname' sn')

--Determines what text needs to be appended to the ref address
--Do not export
getAcc :: RefType -> String
getAcc Tab       = "Table:"
getAcc Lst       = "List:"
getAcc Fig       = "Figure:"
getAcc Sect      = "Sec:"
getAcc (Def dtp) = getDefName dtp
getAcc Assump    = "A:"
getAcc EqnB      = "Eqn:"
getAcc Cite      = "Cite:"
getAcc Blank     = error "Why are we getting the acronym of a Blank?"
getAcc (DeferredCC _) = error "DeferredCC RefType should not be directly used."
getAcc Link      = "Link:"

-- | Automatically create the label for a definition
-- FIXME: Duplicated from Document.hs!
getDefName :: DType -> String
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"

-- FIXME: Duplicated from Document.hs!
repUnd :: Char -> String
repUnd '_' = "."
repUnd c = c : []

-- for when reference address and the display should be different
mkLabelRA' :: String -> String -> RefType -> Label
mkLabelRA' ref shortn rt = mkLabel ref ref shortn rt

-- for when reference address and the display should be the same
mkLabelSame :: String -> RefType ->Label
mkLabelSame i rt = mkLabel i i i rt

mkLabelRAAssump :: String -> String -> Label
mkLabelRAAssump r s = mkLabel (r ++ "Label") r s Assump

mkLabelRAAssump' :: String -> Label
mkLabelRAAssump' rs = mkLabelRAAssump rs rs

mkLabelRASec :: String -> String -> Label
mkLabelRASec r s = mkLabel (r ++ "Label") r s Sect

mkLabelRALst :: String -> String -> Label
mkLabelRALst r s = mkLabel (r ++ "Label") r s Lst

--label constructor for Graphs and Figures
mkLabelRAFig :: String -> Label
mkLabelRAFig x = mkLabel (x ++ "Label") x x Fig

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s
