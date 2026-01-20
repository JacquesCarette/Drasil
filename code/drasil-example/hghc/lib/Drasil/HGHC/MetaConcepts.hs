module Drasil.HGHC.MetaConcepts (progName) where

import Language.Drasil

-- hack... but will have to stay until a progName is not a CI
progName :: CI
progName = commonIdeaWithDict "hghc" (pn "HGHC") "HGHC" []
