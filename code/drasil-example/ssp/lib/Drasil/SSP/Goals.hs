module Drasil.SSP.Goals (goals, identifyCritAndFSGS, determineNormalFGS,
  determineShearFGS) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.SSP.Defs (crtSlpSrf, fsConcept, slice, slope)
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
identifyCritAndFS = S "Identify the" +:+ phrase crtSlpSrf `S.andThe`
  S "corresponding" +:+. phrase fsConcept

determineF :: (NamedIdea a) => a -> Sentence
determineF what = S "Determine the" +:+ phrase what +:+
  S "between each pair of vertical" +:+. D.toSent (pluralNP (slice `ofThePS` slope))
