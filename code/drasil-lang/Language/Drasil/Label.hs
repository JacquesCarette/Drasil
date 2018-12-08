module Language.Drasil.Label (Label, mkLabelRA',
 mkLabelSame, mkEmptyLabel, mkURILabel, getAdd, mkLabelRAAssump', 
 mkLabelRAFig, mkLabelRASec, modifyLabelEqn, mkLabelRALst,
 getDefName, getReqName) where

import Data.Char (isAscii)
import Control.Lens((^.))

import Language.Drasil.Label.Core
import Language.Drasil.Label.Type (LblType(..), getAdd)
import Language.Drasil.Classes (HasRefAddress(getRefAdd))
import Language.Drasil.ShortName (shortname')
import Language.Drasil.RefTypes (RefType(..), DType(..), ReqType(FR, NFR))

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
getAcc (Req rq)  = getReqName rq
getAcc LCh       = "LC:"
getAcc UnCh      = "UC:"
getAcc Assump    = "A:"
getAcc EqnB      = "Eqn:"
getAcc Cite      = "Cite:"
getAcc (Label x) = getAcc x
getAcc Blank     = error "Why are we getting the acronym of a Blank?"
getAcc (DeferredCC _) = error "DeferredCC RefType should not be directly used."
getAcc Link      = "Link:"

getReqName :: ReqType -> String
getReqName FR  = "FR:"
getReqName NFR = "NFR:"

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
mkLabelRA' ref shortn rt = mkLabel (ref ++ "Label") ref shortn rt

-- for when reference address and the display should be the same
mkLabelSame :: String -> RefType ->Label
mkLabelSame iAndRefAndshortn rt = mkLabel (iAndRefAndshortn++"Label") 
  iAndRefAndshortn iAndRefAndshortn rt

mkLabelRAAssump :: String -> String -> Label
mkLabelRAAssump r s = mkLabel (r ++ "Label") r s Assump

mkLabelRAAssump' :: String -> Label
mkLabelRAAssump' rs = mkLabelRAAssump rs rs

mkLabelRASec :: String -> String -> Label
mkLabelRASec r s = mkLabel (r ++ "Label") r s Sect

--FIXME: hack
modifyLabelEqn :: Label -> Label
modifyLabelEqn lb = mkLabel ((getAdd (lb ^. getRefAdd)) ++ "Eqn")
                            (getAdd (lb ^. getRefAdd))
                            (getAdd (lb ^. getRefAdd)) EqnB

mkLabelRALst :: String -> String -> Label
mkLabelRALst r s = mkLabel (r ++ "Label") r s Lst

--label constructor for Graphs and Figures
mkLabelRAFig :: String -> Label
mkLabelRAFig x = mkLabel (x ++ "Label") x x Fig

--mkLabelML :: String -> String -> String -> Label
--mkLabelML i ref shortn = Lbl i (MetaLink $ ensureASCII ref) (shortname' shortn)

--mkLabelURI :: String -> String -> String -> Label
--mkLabelURI i ref shortn = Lbl i (URI $ ensureASCII ref) (shortname' shortn)

ensureASCII :: String -> String
ensureASCII s = map (\y -> if isAscii y then y else error "Label needs to be pure ASCII.") s

--FIXME: remove upon adding labels to all the data types
mkEmptyLabel :: RefType -> Label
mkEmptyLabel rt = mkLabelSame "empty" rt
