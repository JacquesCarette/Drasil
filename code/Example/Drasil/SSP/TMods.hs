module Drasil.SSP.TMods (sspTMods) where

import Prelude hiding (tan)
import Language.Drasil

import Drasil.SSP.Unitals (fs, fx, fy, momntOfBdy,
  genForce, genDisplace, porePressure, normStress,
  shrStress, surfHydroForce, fricAngle, cohesion)
import Drasil.SSP.Defs (slope, factor, factorOfSafety, soil)
import Data.Drasil.SentenceStructures (ofThe, ofThe',
  foldlSent, acroA, getTandS, sAnd, sOf)
import Data.Drasil.Utils (getES)
import Data.Drasil.Quantities.Physics (force, distance, displacement)
import Data.Drasil.Concepts.Documentation (safety, model, source)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Quantities.SolidMechanics (shearRes, mobShear, stffness)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Concepts.Physics (linear, stress, friction)
import Data.Drasil.Quantities.PhysicalProperties (mass)

--------------------------
--  Theoretical Models  --
--------------------------

sspTMods :: [RelationConcept]
sspTMods = [fs_rc, equilibrium, mcShrStrgth, effStress, hookesLaw]

-- 

fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" factorOfSafety fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) $= (C shearRes) / (C mobShear)

fs_desc :: Sentence
fs_desc = foldlSent [
  S "The stability metric of the", phrase slope `sC` S "known as the",
  phrase factor `sOf` phrase safety, sParen (getES fs) `sC`
  S "is determined by", S "ratio" `ofThe` phrase shearForce,
  S "at the base of the", phrase slope, sParen (getES mobShear) `sC`
  S "and the resistive shear", sParen (getES shearRes)]

--
  
equilibrium :: RelationConcept
equilibrium = makeRC "equilibrium" (nounPhraseSP "equilibrium") eq_desc eq_rel

-- FIXME: Atomic "i" is a hack.  But we need to sum over something!
eq_rel :: Relation
eq_rel = foldr ($=) 0 (map summ [fx, fy, momntOfBdy])
  where summ = sum_all (Atomic "i") . C

eq_desc :: Sentence
eq_desc = foldlSent [S "For a body in static equilibrium, the net",
  plural force +:+. S "and net moments acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (acroA 8), S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net moment equal to" `sC` E 0,
  S "also able to be analyzed as a scalar in a 2D problem"]

--
mcShrStrgth :: RelationConcept
mcShrStrgth = makeRC "mcShrStrgth" (nounPhraseSP "Mohr-Coulumb shear strength")
  mcSS_desc mcSS_rel

mcSS_rel :: Relation
mcSS_rel = (C shrStress) $= ((C normStress) * (tan (C fricAngle)) + (C cohesion))

mcSS_desc :: Sentence
mcSS_desc = foldlSent [S "For a", phrase soil, S "under", phrase stress,
  S "it will exert a shear resistive strength based on the",
  S "Coulomb sliding law. The resistive shear is",
  S "the maximum amount of shear a", phrase surface,
  S "can experience while remaining rigid, analogous to",
  S "a maximum" +:+. phrase normForce, S "In this", phrase model, S "the",
  getTandS shrStress, S "is proportional to the product of the",
  phrase normStress, S "on the plane", getES normStress,
  S "with it's static", phrase friction, S "in the angular form" +:+.
  (E $ tan (C fricAngle) $= C surfHydroForce),
  --FIXME: sould say U_s but there is no way to say that yet
  S "The", getES shrStress, S "versus", getES normStress,
  S "relationship is not truly",
  phrase linear `sC` S "but assuming the effective", phrase normForce,
  S "is strong enough it can be approximated with a", phrase linear,
  S "fit", sParen (acroA 9), S "where the cohesion", getES cohesion,
  S "represents the", getES shrStress, S "intercept of the fitted line"]

--

effStress :: RelationConcept
effStress = makeRC "effStress"
  (nounPhraseSP "effective stress") effS_desc effS_rel

effS_rel :: Relation
effS_rel = (C normStress) $= (C normStress) - (C porePressure)

effS_desc :: Sentence --FIXME: these are not normStress but they are sigma.
                      -- Add a prime. Symbol inconsistency 
effS_desc = foldlSent [getES normStress, S "is the total", phrase stress,
  S "a soil", phrase mass,
  S "needs to maintain itself as a rigid collection of particles.",
  phrase source `ofThe'` phrase stress,
  S "can be provided by the soil skeleton", getES normStress `sC`
  S "or by the pore pressure from water within the soil" +:+.
  getES porePressure, S "The", phrase stress,
  S "from the soil skeleton is known as the effective",
  phrase stress, getES normStress, S "and is the difference between the",
  S "total", phrase stress, getES normStress, S "and the pore",
  phrase stress, getES porePressure]

--
hookesLaw :: RelationConcept
hookesLaw = makeRC "hookesLaw"
  (nounPhraseSP "Hooke's law") hksLw_desc hksLw_rel

hksLw_rel :: Relation
hksLw_rel = (C genForce) $= (C stffness) * (C genDisplace)

hksLw_desc :: Sentence
hksLw_desc = foldlSent [S "Description Stiffness", getES stffness, S "is the",
  S "resistance a body others to deformation by", phrase displacement,
  getES genDisplace, S "when subject to a", phrase force, getES genForce `sC`
  S "along the same direction. A body with high stiffness will experience",
  S "little deformation when subject to a", phrase force]
