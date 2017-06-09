module Drasil.SSP.GenDefs where

--import Control.Lens ((^.))
import Prelude hiding (tan)

import Language.Drasil
import Drasil.SSP.Units
--import Data.Drasil.Quantities.SolidMechanics

---------------------------
--  General Definitions  --
---------------------------

sspGenDefs :: [RelationConcept]
sspGenDefs = [normForcEq, bsShrFEq, resShr, mobShr, normShrR]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"
--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium") nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = (C ni) := (Int 0) --FIXME: add the long equation

nmFEq_desc :: Sentence
nmFEq_desc = fixmeS

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium") bShFEq_desc bShFEq_rel

bShFEq_rel :: Relation
bShFEq_rel = (C mobShrI) := (Int 0) --FIXME: add the long equation

bShFEq_desc :: Sentence
bShFEq_desc = fixmeS

--
resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear") resShr_desc resShr_rel

resShr_rel :: Relation
resShr_rel = (C shrResI) := (C nrmFSubWat) :* tan (C fricAngle) :+ (C cohesion) :* (C bi) :* sec (C alpha_i)

resShr_desc :: Sentence
resShr_desc = fixmeS

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr" (nounPhraseSP "mobile shear") mobShr_desc mobShr_rel

mobShr_rel :: Relation
mobShr_rel = (C si) := (C shrResI) :/ (C fs) := 
  ((C nrmFSubWat) :* tan (C fricAngle) :+ (C cohesion) :* (C bi) :* sec (C alpha_i)) :/ (C fs)

mobShr_desc :: Sentence
mobShr_desc = fixmeS

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR" (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel

nmShrR_rel :: Relation
nmShrR_rel = (C intShrForce) := (Int 0)

nmShrR_desc :: Sentence
nmShrR_desc = fixmeS