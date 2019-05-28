module Drasil.SSP.Goals (goals, identifyCritAndFSGS, determineNormalFGS,
  determineShearFGS) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.SentenceStructures (andThe)

import Drasil.SSP.Defs (crtSlpSrf, fs_concept, slice, slope)
import Drasil.SSP.Unitals (intNormForce, intShrForce)

-----------
-- Goals --
-----------

goals :: [ConceptInstance]
goals = [identifyCritAndFSGS, determineNormalFGS, determineShearFGS]

identifyCritAndFSGS :: ConceptInstance
identifyCritAndFSGS = cic "identifyCritAndFS" identifyCritAndFS 
  "Identify-Crit-and-FS" goalStmtDom

determineNormalFGS :: ConceptInstance
determineNormalFGS = cic "determineNormalF" (determineF intNormForce) 
  "Determine-Normal-Forces" goalStmtDom

determineShearFGS :: ConceptInstance
determineShearFGS = cic "determineShearF" (determineF intShrForce) 
  "Determine-Shear-Forces" goalStmtDom

identifyCritAndFS :: Sentence
identifyCritAndFS = S "Identify the" +:+ phrase crtSlpSrf `andThe` 
  S "corresponding" +:+. phrase fs_concept
  
determineF :: (NamedIdea a) => a -> Sentence
determineF what = S "Determine the" +:+ phrase what +:+
  S "between each pair of vertical" +:+ plural slice +:+ S "of the" +:+.
  phrase slope
