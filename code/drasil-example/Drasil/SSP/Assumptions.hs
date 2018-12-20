module Drasil.SSP.Assumptions where

import Language.Drasil

import Drasil.SSP.Defs (slpSrf, slopeSrf, slope,
  soil, soilLyr, soilPrpty, intrslce, slice)
import Drasil.SSP.Unitals (coords, normToShear, numbSlices, scalFunc)

import Data.Drasil.SentenceStructures (ofThe', foldlSent, sAnd)

import Data.Drasil.Concepts.Documentation (assumpDom, condition)
import Data.Drasil.Concepts.Physics (force, stress, strain)
import Data.Drasil.Concepts.Math (surface, unit_)
import Data.Drasil.Concepts.SolidMechanics (shearForce)

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10, newA11, newA12]

-- FIXME: Remove the newA AssumpChunk's once ConceptInstance and SCSProg's
-- Assumptions has been migrated to using assumpDom

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10, newA11,  
  newA12 :: AssumpChunk
assumpSSC, assumpFOSL, assumpSLH, assumpSP, assumpSLI, assumpINSFL,
  assumpPSC, assumpENSL, assumpSBSBISL, assumpES, assumpSF, 
  assumpSL :: ConceptInstance
newA1 = assump (makeAssumpRef "Slip-Surface-Concave") monotonicF
assumpSSC = cic "assumpSSC" monotonicF "Slip-Surface-Concave" assumpDom
newA2 = assump (makeAssumpRef "Factor-of-Safety") slopeS
assumpFOSL = cic "assumpFOS" slopeS "Factor-of-Safety" assumpDom
newA3 = assump (makeAssumpRef "Soil-Layer-Homogeneous") homogeneousL
assumpSLH = cic "assumpSLH" homogeneousL "Soil-Layer-Homogeneous" assumpDom
newA4 = assump (makeAssumpRef "Soil-Properties") propertiesS
assumpSP = cic "assumpSP" propertiesS "Soil-Properties" assumpDom
newA5 = assump (makeAssumpRef "Soil-Layers-Isotropic") isotropicP
assumpSLI = cic "assumpSLI" isotropicP "Soil-Layers-Isotropic" assumpDom
newA6 = assump (makeAssumpRef "Interslice-Norm-Shear-Forces-Linear") linearS
assumpINSFL = cic "assumpINSFL" linearS "Interslice-Norm-Shear-Forces-Linear" assumpDom
newA7 = assump (makeAssumpRef "Plane-Strain-Conditions") planeS
assumpPSC = cic "assumpPSC" planeS "Plane-Strain-Conditions" assumpDom
newA8 = assump (makeAssumpRef "Effective-Norm-Stress-Large") largeN
assumpENSL = cic "assumpENSL" largeN "Effective-Norm-Stress-Large" assumpDom
newA9 = assump (makeAssumpRef "Surface-Base-Slice-between-Interslice-Straight-Lines") straightS
assumpSBSBISL = cic "assumpSBSBISL" straightS "Surface-Base-Slice-between-Interslice-Straight-Lines" assumpDom
newA10 = assump (makeAssumpRef "Edge-Slices") edgeS
assumpES = cic "assumpES" edgeS "Edge-Slices" assumpDom
newA11 = assump (makeAssumpRef "Seismic-Force") seismicF
assumpSF = cic "assumpSF" seismicF "Seismic-Force" assumpDom
newA12 = assump (makeAssumpRef "Surface-Load") surfaceL
assumpSL = cic "assumpSL" surfaceL "Surface-Load" assumpDom

monotonicF, slopeS, homogeneousL, isotropicP, linearS,
  planeS, largeN, straightS, propertiesS, edgeS, seismicF, surfaceL :: Sentence

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

edgeS = foldlSent [S "The", phrase intrslce, plural force, 
  S "at the 0th" `sAnd` ch numbSlices :+: S "th", phrase intrslce,
  S "interfaces are zero"]

seismicF = foldlSent [S "There is no seismic force acting on the slope"]

surfaceL = foldlSent [S "There is no imposed", phrase surface `sC` 
  S "load and therefore no external", phrase force `sC` S "acting on the",
  phrase slope]
