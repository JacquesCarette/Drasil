module Drasil.SSP.TMods where

import Prelude hiding (tan)

import Control.Lens ((^.))

import Language.Drasil
import Drasil.SSP.Units
import Drasil.SSP.Defs
import Data.Drasil.Quantities.SolidMechanics

--------------------------
--  Theoretical Models  --
--------------------------

sspTMods :: [RelationConcept]
sspTMods = [fs_rc, equilibrium, mcShrStrgth, effStress, hooksLaw]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"

-- 

fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" factorOfSafety fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) := (C shearRes) / (C mobShear)

fs_desc :: Sentence
fs_desc = 
  S "The stability metric of the" +:+ phrase slope `sC` S "known as the factor of safety" +:+
  sParen (P $ fs ^. symbol) `sC` S "is determined by the ratio of the" +:+
  S "shear force at the base of the" +:+ phrase slope +:+ sParen (P $ mobShear ^. symbol) `sC` 
  S "and the resistive shear" +:+. sParen (P $ shearRes ^. symbol)

--
  
equilibrium :: RelationConcept
equilibrium = makeRC "equilibrium" (nounPhraseSP "equilibrium") eq_desc eq_rel

eq_rel :: Relation
eq_rel = (UnaryOp $ Summation Nothing (C genForce)) := (Int 0) --FIXME: add net x force, net y force, and net moment

eq_desc :: Sentence
eq_desc = fixmeS

--
mcShrStrgth :: RelationConcept
mcShrStrgth = makeRC "mcShrStrgth" (nounPhraseSP "Mohr-Coulumb shear strength")  mcSS_desc mcSS_rel

mcSS_rel :: Relation
mcSS_rel = (C shrResI) := ((C normStress) :* (tan (C fricAngle)) :+ (C cohesion))

mcSS_desc :: Sentence
mcSS_desc = fixmeS

--

effStress :: RelationConcept
effStress = makeRC "effStress" (nounPhraseSP "effective stress") effS_desc effS_rel

effS_rel :: Relation
effS_rel = (Int 0) := (Int 0) --(P $ Concat [Greek Sigma_L, Atomic "'"]) := (P $ Greek Sigma_L) :- (P $ Greek Mu_L)
--FIXME: add actual symbols once greek alphabet is finished
effS_desc :: Sentence
effS_desc = fixmeS

--
hooksLaw :: RelationConcept
hooksLaw = makeRC "hooksLaw" (nounPhraseSP "Hook's law") hksLw_desc hksLw_rel

hksLw_rel :: Relation
hksLw_rel = (C genForce) := (C stffness) :* (C genDisplace)

hksLw_desc :: Sentence
hksLw_desc = fixmeS