module Drasil.SSP.Assumptions where

import Language.Drasil

import Drasil.SSP.Defs (slpSrf, slopeSrf, slope,
  mtrlPrpty, soil, soilLyr, soilPrpty, intrslce, slice)
import Drasil.SSP.Unitals (coords, normToShear, scalFunc, fs)
import Drasil.SSP.Labels (forDisEqlbL)
import Data.Drasil.SentenceStructures (ofThe, ofThe', getTandS, foldlSent)

import Data.Drasil.Concepts.Documentation (assumpDom, condition)
import Data.Drasil.Concepts.Physics (force, stress, strain)
import Data.Drasil.Concepts.Math (surface, unit_)
import Data.Drasil.Concepts.SolidMechanics (shearForce)

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10]

-- FIXME: Remove the newA AssumpChunk's once ConceptInstance and SCSProg's
-- Assumptions has been migrated to using assumpDom

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10 :: AssumpChunk
assumpSSC, assumpGSMPSL, assumpSLH, assumpSLI, assumpINSFL, assumpBNSFLFS,
  assumpSSCIL, assumpPSC, assumpENSL, assumpSBSBISL :: ConceptInstance
newA1 = assump "Slip-Surface-Concave" monotonicF (mkLabelRAAssump' "Slip-Surface-Concave")
assumpSSC = cic "assumpSSC" monotonicF "Slip-Surface-Concave" assumpDom
newA2 = assump "Geo-Slope-Mat-Props-of-Soil-Inputs" slopeG (mkLabelRAAssump' "Geo-Slope-Mat-Props-of-Soil-Inputs")
assumpGSMPSL = cic "assumpGSMPSL" slopeG "Geo-Slope-Mat-Props-of-Soil-Inputs" assumpDom
newA3 = assump "Soil-Layer-Homogeneous" homogeneousL (mkLabelRAAssump' "Soil-Layer-Homogeneous")
assumpSLH = cic "assumpSLH" homogeneousL "Soil-Layer-Homogeneous" assumpDom
newA4 = assump "Soil-Layers-Isotropic" isotropicP (mkLabelRAAssump' "Soil-Layers-Isotropic")
assumpSLI = cic "assumpSLI" isotropicP "Soil-Layers-Isotropic" assumpDom
newA5 = assump "Interslice-Norm-Shear-Forces-Linear" linearS (mkLabelRAAssump' "Interslice-Norm-Shear-Forces-Linear")
assumpINSFL = cic "assumpINSFL" linearS "Interslice-Norm-Shear-Forces-Linear" assumpDom
newA6 = assump "Base-Norm-Shear-Forces-Linear-on-FS" linearF (mkLabelRAAssump' "Base-Norm-Shear-Forces-Linear-on-FS")
assumpBNSFLFS = cic "assumpBNSFLFS" linearF "Base-Norm-Shear-Forces-Linear-on-FS" assumpDom
newA7 = assump "Stress-Strain-Curve-interslice-Linear" stressC (mkLabelRAAssump' "Stress-Strain-Curve-interslice-Linear")
assumpSSCIL = cic "assumpSSCIL" stressC "Stress-Strain-Curve-interslice-Linear" assumpDom
newA8 = assump "Plane-Strain-Conditions" planeS (mkLabelRAAssump' "Plane-Strain-Conditions")
assumpPSC = cic "assumpPSC" planeS "Plane-Strain-Conditions" assumpDom
newA9 = assump "Effective-Norm-Stress-Large" largeN (mkLabelRAAssump' "Effective-Norm-Stress-Large")
assumpENSL = cic "assumpENSL" largeN "Effective-Norm-Stress-Large" assumpDom
newA10 = assump "Surface-Base-Slice-between-Interslice-Straight-Lines" straightS 
           (mkLabelRAAssump' "Surface-Base-Slice-between-Interslice-Straight-Lines")
assumpSBSBISL = cic "assumpSBSBISL" straightS "Surface-Base-Slice-between-Interslice-Straight-Lines" assumpDom

monotonicF, slopeG, homogeneousL, isotropicP, linearS,
  linearF, stressC, planeS, largeN, straightS :: Sentence

monotonicF = foldlSent [S "The", phrase slpSrf,
  S "is concave with respect to", S "the" +:+. phrase slopeSrf,
  ((ch coords +:+ S "coordinates") `ofThe'` S "failure"),
  phrase surface, S "follow a monotonic function"]

slopeG = foldlSent [S "geometry" `ofThe'` phrase slope `sC` S "and",
  plural mtrlPrpty `ofThe` plural soilLyr, S "are given as inputs",
  sSqBr $ makeRefS forDisEqlbL]

homogeneousL = foldlSent [S "different layers" `ofThe'` phrase soil,
  S "are homogeneous" `sC` S "with consistent", plural soilPrpty,
  S "throughout" `sC` S "and independent of dry or saturated",
  plural condition `sC` S "with the exception of", phrase unit_, S "weight"]

isotropicP = foldlSent [at_start' soilLyr, S "are treated as if they have",
  S "isotropic properties"]

linearS = foldlSent [at_start intrslce, S "normal and", plural shearForce,
  S "have a linear relationship, proportional to a constant",
  sParen (ch normToShear), S "and an", phrase intrslce, phrase force,
  S "function", sParen (ch scalFunc), S "depending on x position"]

linearF = foldlSent [at_start slice, S "to base normal and",
  plural shearForce, S "have", S "a linear relationship, dependent on the",
  getTandS fs `sC` S "and the Coulomb sliding law"]

stressC = foldlSent [S "The stress - strain curve for",
  phrase intrslce, S "relationships is",
  S "linear with a constant", phrase slope]

planeS = foldlSent [S "The", phrase slope, S "and", phrase slpSrf +:+.
  S "extends far into and out of the geometry (z coordinate)",
  S "This implies plane", phrase strain, plural condition `sC`
  S "making 2D analysis appropriate"]

largeN = foldlSent [S "The effective normal", phrase stress,
  S "is large enough that the resistive shear to effective normal",
  phrase stress, S "relationship can be approximated as a",
  S "linear relationship"]

straightS = foldlSent [S "The", phrase surface, S "and base of a",
  phrase slice, S "between", phrase intrslce,
  S "nodes are approximated as straight lines"]
