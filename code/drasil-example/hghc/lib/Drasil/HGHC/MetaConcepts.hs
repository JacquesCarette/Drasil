module Drasil.HGHC.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil

-- hack... but will have to stay until a progName is not a CI
progName :: CI
progName = commonIdeaWithDict (mkUid "hghc") (pn "HGHC") "HGHC" []
