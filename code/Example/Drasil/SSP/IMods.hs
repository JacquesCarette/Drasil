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
sspIMods = [fctSfty]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"
--
fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel

fcSfty_rel :: Relation
fcSfty_rel = (C fs) := (Int 0) --FIXME: add the long equation

fcSfty_desc :: Sentence
fcSfty_desc = fixmeS