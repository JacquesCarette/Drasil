-- | Theory related Drasil concepts, used across Drasil.
module Data.Drasil.TheoryConcepts where

import Language.Drasil (cn', CI, commonIdeaWithDict)

import Drasil.Metadata (softEng)

-- | These are internal-to-Drasil common ideas, and need to be defined at the 
-- same time as theories.
dataDefn, genDefn, inModel, thModel :: CI
-- | Data definition.
dataDefn = commonIdeaWithDict "dataDefn" (cn' "data definition")    "DD"  [softEng]
-- | General definition.
genDefn  = commonIdeaWithDict "genDefn"  (cn' "general definition") "GD"  [softEng]
-- | Instance model.
inModel  = commonIdeaWithDict "inModel"  (cn' "instance model")     "IM"  [softEng]
-- | Theoretical model.
thModel  = commonIdeaWithDict "thModel"  (cn' "theoretical model")  "TM"  [softEng]
