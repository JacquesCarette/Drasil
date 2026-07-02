{-# LANGUAGE PostfixOperators #-}
module Drasil.BinaryStar.Assumptions (assumptions,
  twoBody, isolated, newtonianGravity, nonRelativistic, pointMass,
  constantMass, inertialFrame, planar, nonzeroSeparation) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import qualified Drasil.SRS.Concepts as SRS (valsOfAuxCons)
import Language.Drasil.Document (fromSources, namedRef)

import Data.Drasil.Concepts.Documentation (assumpDom, consVals)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import Data.Drasil.Quantities.Physics (gravitationalConst)

import Drasil.BinaryStar.Concepts (gravInteraction, newtonLUG)
import Drasil.BinaryStar.Unitals (mass_1, mass_2, sepDist)

assumptions :: [ConceptInstance]
assumptions = [twoBody, isolated, newtonianGravity, nonRelativistic,
  pointMass, constantMass, inertialFrame, planar, nonzeroSeparation]

twoBody, isolated, newtonianGravity, nonRelativistic, pointMass,
  constantMass, inertialFrame, planar, nonzeroSeparation :: ConceptInstance

twoBody = cic "twoBody"
  (S "The system consists of exactly two stars with masses" +:+
   ch mass_1 `S.and_` ch mass_2 +:+. S "" +:+
   S "Perturbations from third-body" +:+ phrase gravity +:+
   S "are ignored" !.)
  "twoBody" assumpDom

isolated = cic "isolated"
  (S "Non-gravitational forces (e.g., drag, thrust, radiation pressure)" +:+
   S "are neglected; the only interaction modeled" `S.is`
   S "mutual" +:+ phrase gravity +:+
   S "between the two stars" +:+. fromSource newtonianGravity)
  "isolated" assumpDom

newtonianGravity = cic "newtonianGravity"
  (S "The" +:+ phrase gravInteraction +:+
   S "between the stars is modeled using" +:+
   phrase newtonLUG `sC`
   S "with the" +:+ phrase gravitationalConst +:+
   ch gravitationalConst +:+ S "provided in" +:+.
   namedRef (SRS.valsOfAuxCons [] []) (titleize consVals))
  "newtonianGravity" assumpDom

nonRelativistic = cic "nonRelativistic"
  (S "The motion is modeled using classical (non-relativistic) mechanics;" +:+
   S "relativistic effects are neglected" !.)
  "nonRelativistic" assumpDom

pointMass = cic "pointMass"
  (S "Each star" +:+
   sParen (ch mass_1 `sC` ch mass_2) +:+
   S "is modeled as a point mass, and effects due to" +:+
   S "stellar size, deformation, or rotation are neglected" !.)
  "pointMass" assumpDom

constantMass = cic "constantMass"
  (S "The masses" +:+ ch mass_1 `S.and_` ch mass_2 +:+
   S "remain constant over time" !.)
  "constantMass" assumpDom

inertialFrame = cic "inertialFrame"
  (S "The simulation is performed in an inertial reference frame" !.)
  "inertialFrame" assumpDom

planar = cic "planar"
  (S "The motion of the binary star system is confined to a" +:+
   phrase twoD +:+ S "plane" !.)
  "planar" assumpDom

nonzeroSeparation = cic "nonzeroSeparation"
  (S "Collisions are out of scope: the separation distance" +:+
   ch sepDist +:+
   S "is positive for all simulated times," +:+
   S "so the gravitational force model remains well-defined" +:+.
   fromSources [newtonianGravity, pointMass])
  "nonzeroSeparation" assumpDom
