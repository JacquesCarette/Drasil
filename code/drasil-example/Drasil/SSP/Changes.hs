module Drasil.SSP.Changes (likelyChanges_SRS, unlikelyChanges_SRS) where

-- A list of likely and unlikely changes for the SSP example

import Language.Drasil
import Drasil.DocLang (mkLklyChnk, mkUnLklyChnk)

import Data.Drasil.Concepts.Documentation (system)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.SentenceStructures (foldlSent, foldlSP, sAnd)

import Drasil.SSP.Assumptions (newA3, newA5, newA6, newA8)

likelyChanges_SRS :: [LabelledContent]
likelyChanges_SRS = [likelychg1]

likelychg1 :: LabelledContent
likelychg1 = mkLklyChnk "LC_inhomogeneous" lc1Desc "Calculate-Inhomogeneous-Soil-Layers"

lc1Desc :: Sentence
lc1Desc = foldlSent [(makeRef newA3) `sDash` S "The",
  phrase system +:+. S "currently assumes the different layers of the soil are homogeneous",
  S "In the future,", plural calculation,
  S "can be added for inconsistent soil properties throughout"]

unlikelyChanges_SRS :: [Contents]
unlikelyChanges_SRS = [ucIntro, LlC unlikelychg1, LlC unlikelychg2]

unlikelychg1, unlikelychg2 :: LabelledContent

unlikelychg1 = mkUnLklyChnk "UC_normshearlinear" uc1Desc "Normal-And-Shear-Linear-Only"
unlikelychg2 = mkUnLklyChnk "UC_2donly"          uc2Desc "2D-Analysis-Only"

uc1Desc, uc2Desc :: Sentence

uc1Desc = foldlSent [S "Changes related to", (makeRef newA5) `sAnd`
  (makeRef newA6), S "are not possible due to the dependency",
  S "of the", plural calculation, S "on the linear relationship between",
  S "interslice normal and shear forces"]

uc2Desc = foldlSent [makeRef newA8, S "allows for 2D analysis" +:+.
  S "with these models only because stress along z-direction is zero", 
  S "These models do not take into account stress in the z-direction, and",
  S "therefore cannot be without manipulation to attempt 3d analysis"]

ucIntro :: Contents
ucIntro = foldlSP [S "If changes were to be made with regard to the following" `sC`
  S "a different algorithm would be needed"]