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
sspGenDefs = [normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, netForce, hooksLaw2d, displVect]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"
--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium") nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = C totNrmForce := (Int 0) --FIXME: add the long equation

nmFEq_desc :: Sentence
nmFEq_desc = fixmeS

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium") bShFEq_desc bShFEq_rel

bShFEq_rel :: Relation
bShFEq_rel = C mobShrI := (Int 0) --FIXME: add the long equation

bShFEq_desc :: Sentence
bShFEq_desc = fixmeS

--
resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear") resShr_desc resShr_rel

resShr_rel :: Relation
resShr_rel = C shrResI := C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)

resShr_desc :: Sentence
resShr_desc = fixmeS

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr" (nounPhraseSP "mobile shear") mobShr_desc mobShr_rel

mobShr_rel :: Relation
mobShr_rel = C mobShrI := C shrResI :/ C fs := 
  (C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)) :/ C fs

mobShr_desc :: Sentence
mobShr_desc = fixmeS

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR" (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel

nmShrR_rel :: Relation
nmShrR_rel = C intShrForce := C normToShear :* C scalFunc :* C intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = fixmeS

--
momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium") momEql_desc momEql_rel

momEql_rel :: Relation
momEql_rel = (Int 0) := (Int 0) --FIXME: add the long equation

momEql_desc :: Sentence
momEql_desc = fixmeS

--
netForce :: RelationConcept
netForce = makeRC "netForce" (nounPhraseSP "net force") fNet_desc fNet_rel

fNet_rel :: Relation
fNet_rel = C genForce := (Int 0) --FIXME: requires two lines of equal signs

fNet_desc :: Sentence
fNet_desc = fixmeS

--
hooksLaw2d :: RelationConcept
hooksLaw2d = makeRC "hooksLaw2d" (nounPhraseSP "Hook's law 2D") hook2d_desc hook2d_rel

hook2d_rel :: Relation
hook2d_rel = (Int 0) := (Int 0) --FIXME: cannot yet generate matricies

hook2d_desc :: Sentence
hook2d_desc = fixmeS

--
displVect :: RelationConcept
displVect = makeRC "displVect" (nounPhraseSP "displacement vectors") disVec_desc disVec_rel

disVec_rel :: Relation
disVec_rel = (Int 0) := (Int 0) --FIXME: cannot yet generate matricies

disVec_desc :: Sentence
disVec_desc = fixmeS