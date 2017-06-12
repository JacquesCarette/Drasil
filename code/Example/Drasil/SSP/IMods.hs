module Drasil.SSP.IMods where

--import Control.Lens ((^.))

import Language.Drasil
import Drasil.SSP.Units
import Drasil.SSP.Defs
--import Data.Drasil.Quantities.SolidMechanics

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [RelationConcept]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb, rfemFoS, crtSlpId]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"
--
fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel

fcSfty_rel :: Relation
fcSfty_rel = (C fs) := (Int 0) --FIXME: add the long equation

fcSfty_desc :: Sentence
fcSfty_desc = fixmeS

--
nrmShrFor :: RelationConcept
nrmShrFor = makeRC "nrmShrFor" (nounPhraseSP "normal/shear force ratio") nrmShrF_desc nrmShrF_rel

nrmShrF_rel :: Relation
nrmShrF_rel = (C fs) := (Int 0) --FIXME: add the long equation

nrmShrF_desc :: Sentence
nrmShrF_desc = fixmeS

--
intsliceFs :: RelationConcept
intsliceFs = makeRC "intsliceFs" (nounPhraseSP "interslice forces") sliceFs_desc sliceFs_rel

sliceFs_rel :: Relation
sliceFs_rel = (C fs) := (Int 0) --FIXME: add the long equation

sliceFs_desc :: Sentence
sliceFs_desc = fixmeS

--
forDisEqlb :: RelationConcept
forDisEqlb = makeRC "forDisEqlb" (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel

fDisEq_rel :: Relation
fDisEq_rel = (C fs) := (Int 0) --FIXME: add the long equation

fDisEq_desc :: Sentence
fDisEq_desc = fixmeS

--
rfemFoS :: RelationConcept
rfemFoS = makeRC "rfemFoS" (nounPhraseSP "RFEM factor of safety") rfemFoS_desc rfemFoS_rel

rfemFoS_rel :: Relation
rfemFoS_rel = (C fs) := (Int 0) --FIXME: add the long equation

rfemFoS_desc :: Sentence
rfemFoS_desc = fixmeS

--
crtSlpId :: RelationConcept
crtSlpId = makeRC "crtSlpId" (nounPhraseSP "critical slip identification") crtSlpId_desc crtSlpId_rel

crtSlpId_rel :: Relation
crtSlpId_rel = (C fs) := (C minFunction) :*  (C critCoords)
--FIXME: use brackets and comma for this equation rather than :*

crtSlpId_desc :: Sentence
crtSlpId_desc = fixmeS
