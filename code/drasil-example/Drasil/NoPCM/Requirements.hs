module Drasil.NoPCM.Requirements (funcReqs, inputInitValsTable) where

import Language.Drasil
import Drasil.DocLang (mkInputPropsTable)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (funcReqDom, input_, value)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.SWHS.DataDefs (balanceDecayRate)
import Drasil.SWHS.Requirements (calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime,
  checkWithPhysConsts, findMassConstruct, inReqDesc, oIDQConstruct)
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
inputInitVals :: ConceptInstance
inputInitVals = cic "inputInitVals" ( foldlSent [
  titleize input_, S "the following", plural value, S "described in",
  makeRef2S inputInitValsTable `sC` S "which define", inReqDesc])
  "Input-Initial-Values" funcReqDom

--
findMassExpr :: Expr
findMassExpr = sy wMass $= sy wVol * sy wDensity $=
  (sy pi_ * ((sy diam / 2) $^ 2) * sy tankLength * sy wDensity)

findMass :: ConceptInstance
findMass = findMassConstruct inputInitVals (phrase mass) [eBalanceOnWtr]
              (E findMassExpr) (ch wVol `isThe` phrase wVol)

--
oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [S "the", plural value, S "from", makeRef2S inputInitVals],
  [S "the", phrase mass, S "from", makeRef2S findMass],
  [ch balanceDecayRate, sParen (S "from" +:+ makeRef2S balanceDecayRate)]
  ]

inputInitValsTable :: LabelledContent
inputInitValsTable = mkInputPropsTable inputs inputInitVals

funcReqs :: [ConceptInstance]
funcReqs = [inputInitVals, findMass, checkWithPhysConsts,
        oIDQConstruct oIDQVals, calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
