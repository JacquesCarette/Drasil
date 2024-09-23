module Drasil.SSP.Changes (likelyChgs, unlikelyChgs) where

-- A list of likely and unlikely changes for the SSP example

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (analysis, likeChgDom, model, system, unlikeChgDom)
import Data.Drasil.Concepts.Math (calculation, zDir)
import Data.Drasil.Concepts.Physics (force, stress, threeD, twoD)

import Drasil.SSP.Assumptions (assumpSLH, assumpINSFL, assumpENSL, 
  assumpSF, assumpSL)
import Drasil.SSP.Defs (slope, soil, soilPrpty)
import Drasil.SSP.Unitals (intNormForce, intShrForce, surfLoad)

likelyChgs :: [ConceptInstance]
likelyChgs = [likelyChgCISL, likelyChgCSF, likelyChgCEF]

likelyChgCISL :: ConceptInstance
likelyChgCISL = cic "LC_inhomogeneous" lcCISLDesc "Calculate-Inhomogeneous-Soil-Layers" likeChgDom

likelyChgCSF :: ConceptInstance
likelyChgCSF = cic "LC_seismic" lcCSFDesc "Calculate-Seismic-Force" likeChgDom

likelyChgCEF :: ConceptInstance
likelyChgCEF = cic "LC_external" lcCEFDesc "Calculate-External-Force" likeChgDom

lcCISLDesc :: Sentence
lcCISLDesc = foldlSent [chgsStart assumpSLH (S "The"), phrase system,
  S "currently assumes the", phrase soil +:+. S "mass is homogeneous",
  S "In the future" `sC` plural calculation,
  S "can be added for inconsistent", plural soilPrpty, S "throughout"]

lcCSFDesc :: Sentence
lcCSFDesc = foldlSent [chgsStart assumpSF (S "The"), phrase system,
  S "currently assumes no seismic" +:+. phrase force, S "In the future" `sC`
  plural calculation, S "can be added" `S.for` S "the presence of seismic", phrase force]

lcCEFDesc :: Sentence
lcCEFDesc = foldlSent [chgsStart assumpSL (S "The"), phrase system,
  S "currently assumes no" +:+. phrase surfLoad, S "In the future" `sC`
  plural calculation, S "can be added" `S.for` S "an imposed surface load on the", 
  phrase slope]

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikelyChgNISLO, unlikelyChg2AO]

unlikelyChgNISLO, unlikelyChg2AO :: ConceptInstance

unlikelyChgNISLO = cic "UC_normshearlinear" ucNASLODesc "Normal-And-Shear-Linear-Only" unlikeChgDom
unlikelyChg2AO   = cic "UC_2donly"          uc2AODesc   "2D-Analysis-Only"             unlikeChgDom

ucNASLODesc, uc2AODesc :: Sentence

ucNASLODesc = foldlSent [S "Changes related to",
  refS assumpINSFL, S "are not possible due to the dependency"
  `S.ofThe` plural calculation, S "on the linear relationship between",
  phraseNP (intNormForce `and_` intShrForce)]

uc2AODesc = foldlSent [refS assumpENSL, S "allows for", short twoD, 
  phrase analysis, S "with these", plural model, S "only because", 
  phrase stress, S "along the" +:+. (phrase zDir `S.is` S "zero"), 
  S "These", plural model, S "do not take into account", phraseNP (stress 
  `inThe` zDir) `sC` S "and therefore cannot be used",
  S "without manipulation to attempt", phraseNP (combineNINI threeD analysis)]