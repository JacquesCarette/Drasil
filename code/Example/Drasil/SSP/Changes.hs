module Drasil.SSP.Changes (likelyChanges_SRS) where

-- A list of likely and unlikely changes for the SSP example

import Language.Drasil
import Drasil.DocumentLanguage (mkLklyChnk)
import Data.Drasil.SentenceStructures (foldlSent)
import Drasil.SSP.Assumptions (sspRefDB, newA3)
import Data.Drasil.Concepts.Math (calculation)
import Drasil.DocumentLanguage.RefHelpers (refA)
import Data.Drasil.Concepts.Documentation (system)

likelyChanges_SRS :: [Contents]
likelyChanges_SRS = [likelychg1]

likelychg1 :: Contents
likelychg1 = mkLklyChnk "LC_inhomogeneous" lc1Desc "Calculate-Inhomogeneous-Soil-Layers"

lc1Desc :: Sentence
lc1Desc = foldlSent [(refA sspRefDB newA3) `sDash` S "The",
  phrase system +:+. S "currently assumes the different layers of the soil are homogeneous",
  S "In the future,", plural calculation,
  S "can be added for inconsistent soil properties throughout"]

--unlikelyChanges_SRS :: [Contents]