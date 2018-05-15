module Drasil.SSP.Assumptions (sspAssumptions, assumps_SSP_list_new) where

import Language.Drasil

import Drasil.SSP.Defs (slpSrf, slopeSrf, slope,
  mtrlPrpty, soil, soilLyr, soilPrpty, intrslce, slice)
import Drasil.SSP.Unitals (coords, normToShear, scalFunc, fs)

import Data.Drasil.Utils (getES)
import Data.Drasil.SentenceStructures (ofThe, ofThe', getTandS, foldlSent)

import Data.Drasil.Concepts.Documentation (condition)
import Data.Drasil.Concepts.Physics (force, stress, strain)
import Data.Drasil.Concepts.Math (surface, unit_)
import Data.Drasil.Concepts.SolidMechanics (shearForce)

assumps_SSP_list_new :: [AssumpChunk]
assumps_SSP_list_new = [assump_new_1, assump_new_2,assump_new_3, assump_new_4, assump_new_5,
  assump_new_6, assump_new_7, assump_new_8, assump_new_9, assump_new_10]

assump_new_1 = assump "assump1" monotonicF (S "assump1");
assump_new_2 = assump "assump2" slopeG (S "assump2");
assump_new_3 = assump "assump3" homogeneousL (S "assump3");
assump_new_4 = assump "assump4" isotropicP (S "assump4");
assump_new_5 = assump "assump5" linearS (S "assump5");
assump_new_6 = assump "assump6" linearF (S "assump6");
assump_new_7 = assump "assump7" stressC (S "assump7");
assump_new_8 = assump "assump8" planeS (S "assump8");
assump_new_9 = assump "assump9" largeN (S "assump9");
assump_new_10 = assump "assump10" straightS (S "assump10");

sspAssumptions :: [Sentence]
sspAssumptions = [monotonicF, slopeG, homogeneousL, isotropicP,
  linearS, linearF, stressC, planeS, largeN, straightS]

monotonicF, slopeG, homogeneousL, isotropicP, linearS,
  linearF, stressC, planeS, largeN, straightS :: Sentence

monotonicF = foldlSent [S "The", phrase slpSrf,
  S "is concave with respect to", S "the" +:+. phrase slopeSrf,
  ((getES coords +:+ S "coordinates") `ofThe'` S "failure"),
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
  sParen (getES normToShear), S "and an", phrase intrslce, phrase force,
  S "function", sParen (getES scalFunc), S "depending on x position"]

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