module Drasil.SSP.Assumptions where

import Language.Drasil

import Drasil.SSP.Defs
import Drasil.SSP.Unitals

import Data.Drasil.Utils
import Data.Drasil.SentenceStructures

import Data.Drasil.Concepts.Documentation (condition)
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.SolidMechanics (shearForce)

sspAssumptions :: [Sentence]
sspAssumptions = [monotonicF, slopeG, homogeneousL, isotropicP,
  linearS, linearF, stressC, planeS, largeN, straightS]

monotonicF, slopeG, homogeneousL, isotropicP, linearS,
  linearF, stressC, planeS, largeN, straightS :: Sentence

monotonicF = S "The" +:+ phrase slpSrf +:+ S "is concave with respect to" +:+
  S "the" +:+. phrase slopeSrf +:+ ((getS coords +:+
  S "coordinates") `ofThe'` S "failure") +:+ phrase surface +:+.
  S "follow a monotonic function"

slopeG = S "geometry" `ofThe'` phrase slope `sC` S "and" +:+
  (plural mtrlPrpty `ofThe` plural soilLyr) +:+.
  S "are given as inputs"

homogeneousL = S "different layers" `ofThe'` phrase soil +:+ S "are homogeneous" `sC`
  S "with consistent" +:+ plural soilPrpty +:+ S "throughout" `sC`
  S "and independent of dry or saturated" +:+ plural condition `sC`
  S "with the exception of" +:+ phrase unit_ +:+. S "weight"

isotropicP = at_start' soilLyr +:+ S "are treated as if they have" +:+.
  S "isotropic properties"

linearS = at_start intrslce +:+ S "normal and" +:+ plural shearForce +:+ S "have a" +:+
  S "linear relationship, proportional to a constant" +:+
  sParen (getS normToShear) +:+ S "and an" +:+
  phrase intrslce +:+ phrase force +:+ S "function" +:+ sParen (getS scalFunc) +:+.
  S "depending on x position"

linearF = at_start slice +:+ S "to base normal and" +:+ plural shearForce +:+ S "have" +:+
  S "a linear relationship, dependent on the" +:+
  getTandS fs `sC` S "and the Coulomb sliding law."

stressC = S "The" +:+ phrase stress :+: S "-" :+: phrase strain +:+ S "curve for" +:+ --FIXME: add hypens to drasil language
  phrase intrslce +:+ S "relationships is linear with a constant" +:+.
  phrase slope

planeS = S "The" +:+ phrase slope +:+ S "and" +:+ phrase slpSrf +:+.
  S "extends far into and out of the geometry (z coordinate)" +:+
  S "This implies plane" +:+ phrase strain +:+ plural condition `sC`
  S "making 2D analysis appropriate."

largeN = S "The effective normal" +:+ phrase stress +:+ S "is large enough" +:+
  S "that the resistive shear to effective normal" +:+
  phrase stress +:+ S "relationship can be approximated as a" +:+.
  S "linear relationship"

straightS = S "The" +:+ phrase surface +:+ S "and base of a" +:+
  phrase slice +:+ S "between" +:+ phrase intrslce +:+.
  S "nodes are approximated as straight lines"