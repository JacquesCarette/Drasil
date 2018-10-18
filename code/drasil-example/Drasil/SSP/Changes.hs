module Drasil.SSP.Changes (likelyChanges_SRS, likelyChgs, unlikelyChanges_SRS,
  unlikelyChgs) where

-- A list of likely and unlikely changes for the SSP example

import Language.Drasil
import Drasil.DocLang (mkEnumSimpleD)

import Data.Drasil.Concepts.Documentation (likeChgDom, system, unlikeChgDom)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.SentenceStructures (foldlSent, foldlSP, sAnd)

import Drasil.SSP.Assumptions (newA3, newA5, newA6, newA8)

likelyChanges_SRS :: [Contents]
likelyChanges_SRS = mkEnumSimpleD likelyChgs

likelyChgs :: [ConceptInstance]
likelyChgs = [likelyChgCISL]

likelyChgCISL :: ConceptInstance
likelyChgCISL = cic "LC_inhomogeneous" lcCISLDesc "Calculate-Inhomogeneous-Soil-Layers" likeChgDom

lcCISLDesc :: Sentence
lcCISLDesc = foldlSent [(makeRef newA3) +:+ S "- The",
  phrase system +:+. S "currently assumes the different layers of the soil are homogeneous",
  S "In the future,", plural calculation,
  S "can be added for inconsistent soil properties throughout"]

unlikelyChanges_SRS :: [Contents]
unlikelyChanges_SRS = [ucIntro] ++ mkEnumSimpleD unlikelyChgs

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [unlikelyChgNISLO, unlikelyChg2AO]

unlikelyChgNISLO, unlikelyChg2AO :: ConceptInstance

unlikelyChgNISLO = cic "UC_normshearlinear" ucNASLODesc "Normal-And-Shear-Linear-Only" unlikeChgDom
unlikelyChg2AO =   cic "UC_2donly"          uc2AODesc   "2D-Analysis-Only"             unlikeChgDom

ucNASLODesc, uc2AODesc :: Sentence

ucNASLODesc = foldlSent [S "Changes related to", (makeRef newA5) `sAnd`
  (makeRef newA6), S "are not possible due to the dependency",
  S "of the", plural calculation, S "on the linear relationship between",
  S "interslice normal and shear forces"]

uc2AODesc = foldlSent [makeRef newA8, S "allows for 2D analysis" +:+.
  S "with these models only because stress along z-direction is zero", 
  S "These models do not take into account stress in the z-direction, and",
  S "therefore cannot be without manipulation to attempt 3d analysis"]

ucIntro :: Contents
ucIntro = foldlSP [S "If changes were to be made with regard to the following" `sC`
  S "a different algorithm would be needed"]
