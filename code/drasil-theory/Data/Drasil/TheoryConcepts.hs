module Data.Drasil.TheoryConcepts where

import Language.Drasil (cn', CI, commonIdeaWithDict)
import Data.Drasil.Domains (softEng)

-- These are internal-to-Drasil common ideas, and need to be defined at the 
-- same time as theories.

dataDefn, genDefn, inModel, thModel :: CI

dataDefn = commonIdeaWithDict "dataDefn" (cn' "data definition")    "DD"  [softEng]
genDefn  = commonIdeaWithDict "genDefn"  (cn' "general definition") "GD"  [softEng]
inModel  = commonIdeaWithDict "inModel"  (cn' "instance model")     "IM"  [softEng]
thModel  = commonIdeaWithDict "thModel"  (cn' "theoretical model")  "TM"  [softEng]
