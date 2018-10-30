module Drasil.SSP.Goals (sspGoals) where

import Language.Drasil

import Drasil.SSP.Defs (crtSlpSrf, fs_concept, slope, slpSrf)

import Data.Drasil.SentenceStructures (ofThe)
-----------
-- Goals --
-----------

sspGoals :: [Sentence]
sspGoals = [evaluateFS, lowestFS]

evaluateFS, lowestFS :: Sentence

-- 1
evaluateFS = S "Evaluate the" +:+ phrase fs_concept +:+
  S "along a given" +:+. phrase slpSrf
  
-- 2
lowestFS   = S "Identify the" +:+ phrase crtSlpSrf +:+ S "for the" +:+
  phrase slope `sC` S "with the lowest" +:+. phrase fs_concept
