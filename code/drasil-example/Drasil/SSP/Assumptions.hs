module Drasil.SSP.Assumptions where

import Language.Drasil

import Drasil.SSP.Defs (slpSrf, slopeSrf, slope,
  mtrlPrpty, soil, soilLyr, soilPrpty, intrslce, slice)
import Drasil.SSP.Unitals (coords, normToShear, scalFunc, fs)
import Drasil.SSP.References (sspCitations)
import Data.Drasil.SentenceStructures (ofThe, ofThe', getTandS, foldlSent)

import Data.Drasil.Concepts.Documentation (condition)
import Data.Drasil.Concepts.Physics (force, stress, strain)
import Data.Drasil.Concepts.Math (surface, unit_)
import Data.Drasil.Concepts.SolidMechanics (shearForce)

sspRefDB :: ReferenceDB
sspRefDB = rdb [] [] newAssumptions [] [] sspCitations []
-- FIXME: Convert the rest to new chunk types (similar to issues #446 and #447)

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10]

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10 :: AssumpChunk
newA1 = assump "Slip-Surface-Concave" monotonicF (mkLabelRA'' "Slip-Surface-Concave")
newA2 = assump "Geo-Slope-Mat-Props-of-Soil-Inputs" slopeG (mkLabelRA'' "Geo-Slope-Mat-Props-of-Soil-Inputs")
newA3 = assump "Soil-Layer-Homogeneous" homogeneousL (mkLabelRA'' "Soil-Layer-Homogeneous")
newA4 = assump "Soil-Layers-Isotropic" isotropicP (mkLabelRA'' "Soil-Layers-Isotropic")
newA5 = assump "Interslice-Norm-Shear-Forces-Linear" linearS (mkLabelRA'' "Interslice-Norm-Shear-Forces-Linear")
newA6 = assump "Base-Norm-Shear-Forces-Linear-on-FS" linearF (mkLabelRA'' "Base-Norm-Shear-Forces-Linear-on-FS")
newA7 = assump "Stress-Strain-Curve-interslice-Linear" stressC (mkLabelRA'' "Stress-Strain-Curve-interslice-Linear")
newA8 = assump "Plane-Strain-Conditions" planeS (mkLabelRA'' "Plane-Strain-Conditions")
newA9 = assump "Effective-Norm-Stress-Large" largeN (mkLabelRA'' "Effective-Norm-Stress-Large")
newA10 = assump "Surface-Base-Slice-between-Interslice-Straight-Lines" straightS 
           (mkLabelRA'' "Surface-Base-Slice-between-Interslice-Straight-Lines")

monotonicF, slopeG, homogeneousL, isotropicP, linearS,
  linearF, stressC, planeS, largeN, straightS :: Sentence

monotonicF = foldlSent [S "The", phrase slpSrf,
  S "is concave with respect to", S "the" +:+. phrase slopeSrf,
  ((ch coords +:+ S "coordinates") `ofThe'` S "failure"),
  phrase surface, S "follow a monotonic function"]

slopeG = foldlSent [S "geometry" `ofThe'` phrase slope `sC` S "and",
  plural mtrlPrpty `ofThe` plural soilLyr, S "are given as inputs"]

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

stressC = foldlSent [S "The", phrase stress `sDash` phrase strain,
  S "curve for", phrase intrslce, S "relationships is",
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
