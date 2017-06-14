module Drasil.SSP.TMods where

import Prelude hiding (tan)

import Language.Drasil
import Drasil.SSP.Units
import Drasil.SSP.Defs
import Data.Drasil.Quantities.SolidMechanics
import Data.Drasil.SentenceStructures
import Data.Drasil.Utils
import Data.Drasil.Quantities.Physics
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (normal)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Concepts.Physics (linear, stress, friction)

--------------------------
--  Theoretical Models  --
--------------------------

sspTMods :: [RelationConcept]
sspTMods = [fs_rc, equilibrium, mcShrStrgth, effStress, hooksLaw]

-- 

fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" factorOfSafety fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) := (C shearRes) / (C mobShear)

fs_desc :: Sentence
fs_desc = 
  S "The stability metric of the" +:+ phrase slope `sC` S "known as the" +:+
  plural factor +:+ S "of" +:+ phrase safety +:+ sParen (getS fs) `sC`
  S "is determined by" +:+ (S "ratio" `ofThe` phrase shearForce) +:+
  S "at the base of the" +:+ phrase slope +:+ sParen (getS mobShear) `sC`
  S "and the resistive shear" +:+. sParen (getS shearRes)

--
  
equilibrium :: RelationConcept
equilibrium = makeRC "equilibrium" (nounPhraseSP "equilibrium") eq_desc eq_rel

eq_rel :: Relation
eq_rel = UnaryOp (Summation Nothing (C genForce)) := (Int 0) --FIXME: add net x force, net y force, and net moment

eq_desc :: Sentence
eq_desc = S "For a body in static equilibrium, the net" +:+ plural force +:+
  S "and net moments acting on the body will cancel out. Assuming a 2D problem" +:+
  sParen (acroA "8") +:+ S "the net x-ordinate (Fx)" +:+
  S "and y-ordinate (Fy) scalar components will be equal to 0. All" +:+ plural force +:+
  S "and their" +:+ phrase distance +:+ S "from the chosen point of rotation will create a" +:+
  S "net moment equal to 0, also able to be analyzed as a scalar in a 2D problem."

--
mcShrStrgth :: RelationConcept
mcShrStrgth = makeRC "mcShrStrgth" (nounPhraseSP "Mohr-Coulumb shear strength")  mcSS_desc mcSS_rel

mcSS_rel :: Relation --FIXME: Should be P with no subscript i
mcSS_rel = (C shrResI) := ((C normStress) :* (tan (C fricAngle)) :+ (C cohesion))

mcSS_desc :: Sentence
mcSS_desc = foldlSent [S "For a" +:+ phrase soil +:+ S "under" +:+ phrase stress +:+
  S "it will exert a shear resistive strength based on the",
  S "Coulomb sliding law. The resistive shear is the maximum amount of shear a",
  S "surface can experience while remaining rigid, analogous to a maximum" +:+.
  phrase normForce, S "In this", phrase model, S "the", phrase shearForce,
  getS shrResI, S "is proportional to the product of the",
  phrase normal, phrase stress, S "on the plane", getS normStress,
  S "with it's static", phrase friction, S "in the angular form" +:+.
  (E $ (tan (C fricAngle)) := (C surfHydroForce)), --FIXME: sould say U_s but there is no way to say that yet
  S "The", getS shrResI, S "versus", getS normStress, S "relationship is not truly",
  phrase linear `sC` S "but assuming the effective", phrase normForce,
  S "is strong enough it can be approximated with a", phrase linear,
  S "fit", sParen (acroA "9"), S "where the cohesion", getS cohesion,
  S "represents the", getS shrResI, S "intercept of the fitted line"]

--

effStress :: RelationConcept
effStress = makeRC "effStress" (nounPhraseSP "effective stress") effS_desc effS_rel

effS_rel :: Relation
effS_rel = (C normStress) := (C normStress) :- (C porePressure)

effS_desc :: Sentence -- FIXME: these are not normStress but they are sigma. And some of these are mu. Also fix equaiton
effS_desc = foldlSent [getS normStress, S "is the total stress a soil mass needs to maintain itself as a rigid collection",
  S "of particles. The source of the stress can be provided by the soil skeleton", getS normStress `sC`
  S "or by the pore pressure from water within the soil" +:+. getS porePressure, S "The stress from the soil",
  S "skeleton is known as the effective stress", getS normStress, S "and is the difference between the",
  S "total stress", getS normStress, S "and the pore stress", getS porePressure]

--
hooksLaw :: RelationConcept
hooksLaw = makeRC "hooksLaw" (nounPhraseSP "Hook's law") hksLw_desc hksLw_rel

hksLw_rel :: Relation
hksLw_rel = (C genForce) := (C stffness) :* (C genDisplace)

hksLw_desc :: Sentence
hksLw_desc = foldlSent [S "Description Stiffness", getS stffness, S "is the",
  S "resistance a body others to deformation by displacement", getS genDisplace,
  S "when subject to a force", getS genForce `sC` S "along the same direction.",
  S "A body with high stiffness will experience little deformation when",
  S "subject to a force"]