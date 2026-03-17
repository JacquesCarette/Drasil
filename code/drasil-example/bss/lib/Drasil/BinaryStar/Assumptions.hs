{-# LANGUAGE PostfixOperators #-}
module Drasil.BinaryStar.Assumptions (assumptions,
  twoBody, isolated, newtonianGravity, nonRelativistic, pointMass,
  constantMass, inertialFrame, planar, nonzeroSeparation) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumpDom)
assumptions :: [ConceptInstance]
assumptions = [twoBody, isolated, newtonianGravity, nonRelativistic,
  pointMass, constantMass, inertialFrame, planar, nonzeroSeparation]

twoBody, isolated, newtonianGravity, nonRelativistic, pointMass,
  constantMass, inertialFrame, planar, nonzeroSeparation :: ConceptInstance

twoBody = cic "twoBody"
  (S "The system consists of exactly two stars;" +:+
   S "third-body gravitational perturbations are ignored" !.)
  "twoBody" assumpDom

isolated = cic "isolated"
  (S "Non-gravitational forces (e.g., drag, thrust, radiation pressure)" +:+
   S "are neglected; the only force modeled" `S.is`
   S "mutual gravitation between the two stars" !.)
  "isolated" assumpDom

newtonianGravity = cic "newtonianGravity"
  (S "The gravitational interaction between the stars is modeled using" +:+
   S "Newton's law of universal gravitation" !.)
  "newtonianGravity" assumpDom

nonRelativistic = cic "nonRelativistic"
  (S "The motion is modeled using classical (non-relativistic) mechanics;" +:+
   S "relativistic effects are neglected" !.)
  "nonRelativistic" assumpDom

pointMass = cic "pointMass"
  (S "Each star is modeled as a point mass, and effects due to" +:+
   S "stellar size, deformation, or rotation are neglected" !.)
  "pointMass" assumpDom

constantMass = cic "constantMass"
  (S "The masses of the stars remain constant over time" !.)
  "constantMass" assumpDom

inertialFrame = cic "inertialFrame"
  (S "The simulation is performed in an inertial reference frame" !.)
  "inertialFrame" assumpDom

planar = cic "planar"
  (S "The motion of the binary star system is confined to a" +:+
   S "two-dimensional plane (2D)" !.)
  "planar" assumpDom

nonzeroSeparation = cic "nonzeroSeparation"
  (S "Collisions are out of scope: the separation distance" +:+
   S "is positive for all simulated times," +:+
   S "so the gravitational force model remains well-defined" !.)
  "nonzeroSeparation" assumpDom
