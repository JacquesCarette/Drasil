module Drasil.NoPCM.GenDefs (rocTempSimp, genDefs) where

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)

import Drasil.NoPCM.Assumptions (assumpDWCoW, assumpSHECoW)
import Drasil.SWHS.Assumptions (assumpCWTAT)
import Drasil.SWHS.GenDefs (nwtnCooling, rocTempSimpRC, rocTempSimpDeriv)

genDefs :: [GenDefn]
genDefs = [nwtnCooling, rocTempSimp] 

rocTempSimp :: GenDefn
rocTempSimp = gdNoRefs rocTempSimpRC (Nothing :: Maybe UnitDefn)
  (Just $ rocTempSimpDeriv EmptyS [assumpCWTAT, assumpDWCoW, assumpSHECoW])
  "rocTempSimp" [{-Notes-}]
