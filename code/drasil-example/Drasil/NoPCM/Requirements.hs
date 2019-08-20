module Drasil.NoPCM.Requirements (funcReqs, inputInitQuantsTable) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (quantity)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.DocLang (mkInputPropsTable)

import Drasil.SWHS.DataDefs (balanceDecayRate)
import Drasil.SWHS.Requirements (calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime,
  checkWithPhysConsts, findMassConstruct, iIQConstruct, oIDQConstruct)
import Drasil.SWHS.Unitals (diam, tankLength, wDensity, wMass, wVol)

import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.NoPCM.Unitals (inputs)

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

--
inputInitQuants :: ConceptInstance
inputInitQuants = iIQConstruct inputInitQuantsTable

--
findMassExpr :: Expr
findMassExpr = sy wMass $= sy wVol * sy wDensity $=
  (sy pi_ * ((sy diam / 2) $^ 2) * sy tankLength * sy wDensity)

findMass :: ConceptInstance
findMass = findMassConstruct inputInitQuants (phrase mass) (makeRef2S eBalanceOnWtr)
              (E findMassExpr) (ch wVol `isThe` phrase wVol)

--
oIDQQuants :: [Sentence]
oIDQQuants = map foldlSent_ [
  [S "the", plural quantity, S "from", makeRef2S inputInitQuants],
  [S "the", phrase mass, S "from", makeRef2S findMass],
  [ch balanceDecayRate, sParen (S "from" +:+ makeRef2S balanceDecayRate)]
  ]

inputInitQuantsTable :: LabelledContent
inputInitQuantsTable = mkInputPropsTable inputs inputInitQuants

funcReqs :: [ConceptInstance]
funcReqs = [inputInitQuants, findMass, checkWithPhysConsts,
        oIDQConstruct oIDQQuants, calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
