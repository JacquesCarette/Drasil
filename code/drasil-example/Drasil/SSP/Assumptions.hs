module Drasil.SSP.Assumptions where

import Language.Drasil

import Drasil.SSP.Defs (slpSrf, slopeSrf, slope,
  mtrlPrpty, soil, soilLyr, soilPrpty, intrslce, slice)
import Drasil.SSP.Unitals (coords, normToShear, scalFunc, fs)

import Data.Drasil.SentenceStructures (ofThe, ofThe', getTandS, foldlSent)

import Data.Drasil.Concepts.Documentation (assumpDom, condition)
import Data.Drasil.Concepts.Physics (force, stress, strain)
import Data.Drasil.Concepts.Math (surface, unit_)
import Data.Drasil.Concepts.SolidMechanics (shearForce)

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10, newA11]

-- FIXME: Remove the newA AssumpChunk's once ConceptInstance and SCSProg's
-- Assumptions has been migrated to using assumpDom

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10, newA11 :: AssumpChunk
assumpSSC, assumpFOSL, assumpSLH, assumpSLI, assumpINSFL, assumpBNSFLFS,
  assumpSSCIL, assumpPSC, assumpENSL, assumpSBSBISL, assumpSP :: ConceptInstance
newA1 = assump "Slip-Surface-Concave" monotonicF (mkLabelRAAssump' "Slip-Surface-Concave")
assumpSSC = cic "assumpSSC" monotonicF "Slip-Surface-Concave" assumpDom
newA2 = assump "Factor-of-Safety" slopeS (mkLabelRAAssump' "Factor-of-Safety")
assumpFOSL = cic "assumpFOS" slopeS "Factor-of-Safety" assumpDom
newA3 = assump "Soil-Layer-Homogeneous" homogeneousL (mkLabelRAAssump' "Soil-Layer-Homogeneous")
assumpSLH = cic "assumpSLH" homogeneousL "Soil-Layer-Homogeneous" assumpDom
newA4 = assump "Soil-Properties" propertiesS (mkLabelRAAssump' "Soil-Properties")
assumpSP = cic "assumpSP" propertiesS "Soil-Properties" assumpDom
newA5 = assump "Soil-Layers-Isotropic" isotropicP (mkLabelRAAssump' "Soil-Layers-Isotropic")
assumpSLI = cic "assumpSLI" isotropicP "Soil-Layers-Isotropic" assumpDom
newA6 = assump "Interslice-Norm-Shear-Forces-Linear" linearS (mkLabelRAAssump' "Interslice-Norm-Shear-Forces-Linear")
assumpINSFL = cic "assumpINSFL" linearS "Interslice-Norm-Shear-Forces-Linear" assumpDom
newA7 = assump "Plane-Strain-Conditions" planeS (mkLabelRAAssump' "Plane-Strain-Conditions")
assumpPSC = cic "assumpPSC" planeS "Plane-Strain-Conditions" assumpDom
newA8 = assump "Effective-Norm-Stress-Large" largeN (mkLabelRAAssump' "Effective-Norm-Stress-Large")
assumpENSL = cic "assumpENSL" largeN "Effective-Norm-Stress-Large" assumpDom
newA9 = assump "Surface-Base-Slice-between-Interslice-Straight-Lines" straightS 
           (mkLabelRAAssump' "Surface-Base-Slice-between-Interslice-Straight-Lines")
assumpSBSBISL = cic "assumpSBSBISL" straightS "Surface-Base-Slice-between-Interslice-Straight-Lines" assumpDom
newA10 = assump "Seismic-Force" linearF (mkLabelRAAssump' "Seismic-Force")
assumpBNSFLFS = cic "assumpBNSFLFS" linearF "Base-Norm-Shear-Forces-Linear-on-FS" assumpDom
newA11 = assump "Surface-Load" stressC (mkLabelRAAssump' "Surface-Load")
assumpSSCIL = cic "assumpSSCIL" stressC "Surface-Load" assumpDom

monotonicF, slopeS, homogeneousL, isotropicP, linearS,
  linearF, stressC, planeS, largeN, straightS :: Sentence

monotonicF = foldlSent [S "The", phrase slpSrf,
  S "is concave with respect to", S "the" +:+. phrase slopeSrf,
  ((ch coords +:+ S "coordinates") `ofThe'` S "failure"),
  phrase surface, S "follow a monotonic function"]

slopeS = foldlSent [S "The factor of safety is assumed to be constant across a whole",
  phrase slpSrf]

propertiesS = foldlSent [S "The", plural soilPrpty, S "are independent of dry or saturated",
  plural condition `sC` S "with the exception of", phrase unit_, S "weight"]

homogeneousL = foldlSent [S "different layers" `ofThe'` phrase soil,
  S "are homogeneous" `sC` S "with consistent", plural soilPrpty +:+
  S "throughout"]

isotropicP = foldlSent [at_start' soilLyr, S "are treated as if they have",
  S "isotropic properties"]

linearS = foldlSent [at_start intrslce, S "normal and", plural shearForce,
  S "have a linear relationship, proportional to a constant",
  sParen (ch normToShear), S "and an", phrase intrslce, phrase force,
  S "function", sParen (ch scalFunc), S "depending on x position"]

linearF = foldlSent [S "There is no seismic force acting on the slope"]

stressC = foldlSent [S "There is no imposed", phrase surface `sC` 
  S "load and therefore no external force" `sC` S "acting on the",
  phrase slope]

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
