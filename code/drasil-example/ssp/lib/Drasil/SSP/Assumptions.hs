{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.Assumptions where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.SSP.Defs (plnStrn, slpSrf, slopeSrf, slope,
  soil, soilPrpty, intrslce, slice, waterTable)
import Drasil.SSP.Unitals (baseHydroForce, effCohesion, fricAngle, intNormForce,
  intShrForce, normToShear, numbSlices, scalFunc, shrStress, slipDist, slipHght,
  surfHydroForce, surfLoad, xi, zcoord)
import Drasil.SSP.References (morgenstern1965)

import Data.Drasil.Concepts.Documentation (analysis, assumpDom, assumption, 
  condition, constant, effect, interface)
import Data.Drasil.Concepts.Physics (force, position, stress, twoD)
import Data.Drasil.Concepts.Math (surface, unit_)


assumptions :: [ConceptInstance]
assumptions = [assumpSSC, assumpFOSL, assumpSLH, assumpSP, assumpSLI,
  assumpINSFL, assumpPSC, assumpENSL, assumpSBSBISL, assumpES, assumpSF,
  assumpSL, assumpWIBE, assumpWISE, assumpNESSS, assumpHFSM]

assumpSSC, assumpFOSL, assumpSLH, assumpSP, assumpSLI, assumpINSFL,
  assumpPSC, assumpENSL, assumpSBSBISL, assumpES, assumpSF, 
  assumpSL, assumpWIBE, assumpWISE, assumpNESSS, assumpHFSM :: ConceptInstance

assumpSSC = cic "assumpSSC" monotonicF "Slip-Surface-Concave" assumpDom
assumpFOSL = cic "assumpFOS" slopeS "Factor-of-Safety" assumpDom
assumpSLH = cic "assumpSLH" homogeneousL "Soil-Layer-Homogeneous" assumpDom
assumpSP = cic "assumpSP" propertiesS "Soil-Properties" assumpDom
assumpSLI = cic "assumpSLI" isotropicP "Soil-Layers-Isotropic" assumpDom
assumpINSFL = cic "assumpINSFL" linearS "Interslice-Norm-Shear-Forces-Linear" assumpDom
assumpPSC = cic "assumpPSC" planeS "Plane-Strain-Conditions" assumpDom
assumpENSL = cic "assumpENSL" largeN "Effective-Norm-Stress-Large" assumpDom
assumpSBSBISL = cic "assumpSBSBISL" straightS "Surface-Base-Slice-between-Interslice-Straight-Lines" assumpDom
assumpES = cic "assumpES" edgeS "Edge-Slices" assumpDom
assumpSF = cic "assumpSF" seismicF "Seismic-Force" assumpDom
assumpSL = cic "assumpSL" surfaceL "Surface-Load" assumpDom
assumpWIBE = cic "assumpWIBE" waterBIntersect "Water-Intersects-Base-Edge" 
  assumpDom
assumpWISE = cic "assumpWISE" waterSIntersect "Water-Intersects-Surface-Edge" 
  assumpDom
assumpNESSS = cic "assumpNESSS" negligibleSlopeEffect 
  "Negligible-Effect-Surface-Slope-Seismic" assumpDom
assumpHFSM = cic "assumpHFSM" hydrostaticFMidpoint 
  "Hydrostatic-Force-Slice-Midpoint" assumpDom

monotonicF, slopeS, homogeneousL, isotropicP, linearS, planeS, largeN, 
  straightS, propertiesS, edgeS, seismicF, surfaceL, waterBIntersect, 
  waterSIntersect, negligibleSlopeEffect, hydrostaticFMidpoint :: Sentence

monotonicF = foldlSent [atStartNP (the slpSrf),
  S "is concave" `S.wrt` (phraseNP (the slopeSrf) !.), S "The",
  sParen (ch slipDist `sC` ch slipHght), S "coordinates" `S.ofA` 
  phrase slpSrf, S "follow a concave up function"]

slopeS = foldlSent [S "The factor of safety is assumed to be", phrase constant,
  S "across the entire", phrase slpSrf]

homogeneousL = foldlSent [atStartNP (the soil), S "mass is homogeneous" `sC`
  S "with consistent", plural soilPrpty, S "throughout"]

propertiesS = foldlSent [atStartNP' (the soilPrpty), S "are independent of dry or saturated",
  plural condition `sC` S "with the exception of", phrase unit_, S "weight"]

isotropicP = foldlSent [atStartNP (the soil), S "mass is treated as if the", 
  phraseNP (effCohesion `and_` fricAngle), S "are isotropic properties"]

linearS = foldlSent [S "Following the", phrase assumption, S "of Morgenstern",
  S "and Price", sParen (refS morgenstern1965) `sC` 
  phraseNP (intNormForce `and_` intShrForce),
  S "have a proportional relationship, depending on a proportionality",
  phrase constant, sParen (ch normToShear), S "and a function", 
  sParen (ch scalFunc), S "describing variation depending on", ch xi, 
  phrase position]

planeS = foldlSent [atStartNP (NP.the (slope `and_` slpSrf)),
  S "extends far into and out of the geometry" +:+. sParen (ch zcoord +:+ 
  S "coordinate"), S "This implies", pluralNP (combineNINI plnStrn condition) `sC`
  S "making", short twoD, phrase analysis, S "appropriate"]

largeN = foldlSent [S "The effective normal", phrase stress,
  S "is large enough that the", phrase shrStress, S "to effective normal",
  phrase stress, S "relationship can be approximated as a linear relationship"]

straightS = foldlSent [atStartNP (the surface), S "and base of a",
  phrase slice, S "are approximated as straight lines"]

edgeS = foldlSent [atStartNP (the intrslce), plural force, 
  S "at the 0th" `S.and_` ch numbSlices :+: S "th", phrase intrslce,
  plural interface, S "are zero"]

seismicF = foldlSent [S "There is no seismic", phrase force, S "acting on the", phrase slope]

surfaceL = foldlSent [S "There is no imposed", phrase surface, S "load" `sC`
  S "and therefore no", phrase surfLoad `sC` S "acting on the", phrase slope]

waterBIntersect = foldlSent [atStartNP (the waterTable), S "only intersects", 
  S "the base" `S.ofA` phrase slice, S "at an edge" `S.ofThe` phrase slice]

waterSIntersect = foldlSent [atStartNP (the waterTable), S "only intersects", 
  phraseNP (the slopeSrf), S "at the edge of a", phrase slice]

negligibleSlopeEffect = foldlSent [atStartNP (the effect)
  `S.ofThe` S "slope" `S.ofThe` phraseNP (surface `ofThe` soil) `S.onThe` S "seismic",
  phrase force, S "is assumed to be negligible"]

hydrostaticFMidpoint = foldlSent [S "The resultant", phrase surfHydroForce,
  S "act into the midpoint of each", phrase slice, S "surface" `S.andThe`
  S "resultant", phrase baseHydroForce, S "act into the midpoint of each",
  phrase slice, S "base"]