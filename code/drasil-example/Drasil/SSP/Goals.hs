module Drasil.SSP.Goals (sspGoals) where

import Language.Drasil

import Data.Drasil.SentenceStructures (andThe)

import Drasil.SSP.Defs (crtSlpSrf, fs_concept, slice, slope, slpSrf)
import Drasil.SSP.Unitals (intNormForce, intShrForce)

-----------
-- Goals --
-----------

sspGoals :: [Sentence]
sspGoals = [identifyCritAndFS, determineF intNormForce, determineF intShrForce]

identifyCritAndFS :: Sentence
identifyCritAndFS = S "Identify the" +:+ phrase crtSlpSrf `andThe` 
  S "corresponding" +:+. phrase fs_concept
  
determineF :: (NamedIdea a) => a -> Sentence
determineF what = S "Determine the" +:+ phrase what +:+
  S "between each pair of vertical" +:+ plural slice +:+ S "of the" +:+.
  phrase slope
