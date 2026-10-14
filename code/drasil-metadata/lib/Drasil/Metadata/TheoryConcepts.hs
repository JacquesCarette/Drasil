-- | Theory related Drasil concepts, used across Drasil.
module Drasil.Metadata.TheoryConcepts where

import Drasil.Database (mkUid)
import Language.Drasil (cn', CI, commonIdea)

import Drasil.Metadata.Domains (softEng)

-- | These are internal-to-Drasil common ideas, and need to be defined at the
-- same time as theories.
dataDefn, genDefn, inModel, thModel :: CI
-- | Data definition.
dataDefn = commonIdea (mkUid "dataDefn") (cn' "data definition")    "DD"  [softEng]
-- | General definition.
genDefn  = commonIdea (mkUid "genDefn")  (cn' "general definition") "GD"  [softEng]
-- | Instance model.
inModel  = commonIdea (mkUid "inModel")  (cn' "instance model")     "IM"  [softEng]
-- | Theoretical model.
thModel  = commonIdea (mkUid "thModel")  (cn' "theoretical model")  "TM"  [softEng]
