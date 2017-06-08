module Drasil.SSP.TMods where

import Prelude hiding (tan)

import Control.Lens ((^.))

import Language.Drasil
import Drasil.SSP.Units
import Data.Drasil.Quantities.SolidMechanics

--------------------------
--  Theoretical Models  --
--------------------------

sspTMods :: [RelationConcept]
sspTMods = [fs_rc, equilibrium, mcShrStrgth, effStress]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"

-- possibly temporary "factor of safety" hack FIXME?
factor, safety :: NamedChunk
factor = npnc "factor" (cn' "factor")
safety = npnc "safety" (cnIES "safety")

fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" (factor `of_''` safety) fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) := (C shearRes) / (C mobShear)

fs_desc :: Sentence
fs_desc = 
  S "The stability metric of the slope, known as the factor of safety" +:+
  (sParen $ P $ fs ^. symbol) `sC` S "is determined by the ratio of the" +:+
  S "shear force at the base of the slope" +:+ (sParen $ P $ mobShear ^. symbol) `sC` 
  S "and the resistive shear" +:+. (sParen $ P $ shearRes ^. symbol)

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
mcSS_rel = (C pi_f) := ((C normStress) :* (tan (C fricAngle)) :+ (C cohesion))

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



{-
name :: RelationConcept
name = makeRC "" (nounPhraseSP "") mcSS_desc mcSS_rel

mcSS_rel :: Relation
mcSS_rel = 

mcSS_desc :: Sentence
mcSS_desc = fixmeS

-}