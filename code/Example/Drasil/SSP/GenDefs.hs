module Drasil.SSP.GenDefs where

--import Control.Lens ((^.))

import Language.Drasil
import Drasil.SSP.Units
--import Data.Drasil.Quantities.SolidMechanics

---------------------------
--  General Definitions  --
---------------------------

sspGenDefs :: [RelationConcept]
sspGenDefs = [normForcEq, bsShrFEq]

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
bShFEq_rel = (C si) := (Int 0) --FIXME: add the long equation

bShFEq_desc :: Sentence
bShFEq_desc = fixmeS