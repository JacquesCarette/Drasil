module Drasil.SSP.Changes (likelyChanges_SRS, likelyChgs, unlikelyChanges_SRS,
  unlikelyChgs) where

-- A list of likely and unlikely changes for the SSP example

import Language.Drasil
import Drasil.DocLang (mkEnumSimpleD)

import Data.Drasil.Concepts.Documentation (likeChgDom, system, unlikeChgDom)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Physics (force)
import Data.Drasil.SentenceStructures (foldlSent, foldlSP, sAnd)

import Drasil.SSP.Assumptions (assumpSLH, assumpSLI, assumpINSFL, assumpENSL, 
  assumpSF, assumpSL)
import Drasil.SSP.Defs (slope, soil, soilPrpty)
import Drasil.SSP.Unitals (surfLoad)

likelyChanges_SRS :: [Contents]
likelyChanges_SRS = mkEnumSimpleD likelyChgs

likelyChgs :: [ConceptInstance]
likelyChgs = [likelyChgCISL, likelyChgCSF, likelyChgCEF]

likelyChgCISL :: ConceptInstance
likelyChgCISL = cic "LC_inhomogeneous" lcCISLDesc "Calculate-Inhomogeneous-Soil-Layers" likeChgDom

likelyChgCSF :: ConceptInstance
likelyChgCSF = cic "LC_seismic" lcCSFDesc "Calculate-Seismic-Force" likeChgDom

likelyChgCEF :: ConceptInstance
likelyChgCEF = cic "LC_external" lcCEFDesc "Calculate-External-Force" likeChgDom

lcCISLDesc :: Sentence
lcCISLDesc = foldlSent [S "The", phrase system, S "currently assumes the", 
  phrase soil, S "mass is homogeneous" +:+. sParen (makeRef2S assumpSLH),
  S "In the future" `sC` plural calculation,
  S "can be added for inconsistent", plural soilPrpty, S "throughout"]

lcCSFDesc :: Sentence
lcCSFDesc = foldlSent [S "The", phrase system, S "currently assumes no seismic",
  phrase force +:+. sParen (makeRef2S assumpSF), S "In the future" `sC`
  plural calculation, S "can be added for the presence of seismic", phrase force]

lcCEFDesc :: Sentence
lcCEFDesc = foldlSent [S "The", phrase system, S "currently assumes no",
  phrase surfLoad +:+. sParen (makeRef2S assumpSL), S "In the future" `sC`
  plural calculation, S "can be added for an imposed surface load on the", 
  phrase slope]

unlikelyChanges_SRS :: [Contents]
unlikelyChanges_SRS = ucIntro : mkEnumSimpleD unlikelyChgs

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikelyChgNISLO, unlikelyChg2AO]

unlikelyChgNISLO, unlikelyChg2AO :: ConceptInstance

unlikelyChgNISLO = cic "UC_normshearlinear" ucNASLODesc "Normal-And-Shear-Linear-Only" unlikeChgDom
unlikelyChg2AO =   cic "UC_2donly"          uc2AODesc   "2D-Analysis-Only"             unlikeChgDom

ucNASLODesc, uc2AODesc :: Sentence

ucNASLODesc = foldlSent [S "Changes related to", (makeRef2S assumpSLI) `sAnd`
  (makeRef2S assumpINSFL), S "are not possible due to the dependency",
  S "of the", plural calculation, S "on the linear relationship between",
  S "interslice normal and shear forces"]

uc2AODesc = foldlSent [makeRef2S assumpENSL, S "allows for 2D analysis" +:+.
  S "with these models only because stress along z-direction is zero", 
  S "These models do not take into account stress in the z-direction, and",
  S "therefore cannot be without manipulation to attempt 3d analysis"]

ucIntro :: Contents
ucIntro = foldlSP [S "If changes were to be made with regard to the following" `sC`
  S "a different algorithm would be needed"]
