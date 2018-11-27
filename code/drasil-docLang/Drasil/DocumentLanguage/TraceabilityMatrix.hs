module Drasil.DocumentLanguage.TraceabilityMatrix where

import Language.Drasil
import Drasil.DocumentLanguage.RefHelpers (ModelDB, ddRefDB, gdRefDB, imRefDB, 
    modelsFromDB, tmRefDB)

modelTraceTable :: ModelDB -> LabelledContent
modelTraceTable mdb = llcc (mkLabelSame "RefAdd" Tab) $ Table
  ( EmptyS : crossListItems ) [[{-data (rows)-}]] (S "Title") True {-<-- showLabel?-}
  where crossListItems = getRefs tmDB ++ getRefs gdDB ++ getRefs ddDB ++ getRefs imDB
        getRefs db = map makeRef2S (modelsFromDB db)
        tmDB = tmRefDB mdb
        gdDB = gdRefDB mdb
        ddDB = ddRefDB mdb
        imDB = imRefDB mdb
