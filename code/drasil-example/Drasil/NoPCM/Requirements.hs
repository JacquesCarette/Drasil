module Drasil.NoPCM.Requirements (funcReqsList, reqs, dataConstListIn) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation as Doc (description, input_,
  quantity, requirement, symbol_, variable)

import Data.Drasil.Concepts.Math (unit_)
import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.DocLang (mkEnumSimpleD)
import Drasil.DocumentLanguage.Units (toSentence) 
import Data.Drasil.SentenceStructures (foldlSent_)

import Drasil.SWHS.Requirements (calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime,
  checkWithPhysConsts, findMassConstruct, iIQConstruct, oIDQConstruct)
import Drasil.SWHS.Unitals (coilHTC, coilSA, diam, htCapW, tankLength,
  tauW, tempC, timeFinal, wDensity, wMass, wVol, absTol, rel_tol, cons_tol)

import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.NoPCM.Unitals (tempInit)

inputVar :: [QuantityDict]
inputVar = map qw dataConstListIn ++ map qw [absTol, rel_tol, cons_tol]

dataConstListIn :: [UncertQ]
dataConstListIn = [tankLength, diam, coilSA, tempC, wDensity, htCapW,
  coilHTC, tempInit, timeFinal]

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

funcReqsList :: [Contents]
funcReqsList = (mkEnumSimpleD reqs) ++ [LlC inputInitQuantsTable]

--
inputInitQuants :: ConceptInstance
inputInitQuants = iIQConstruct inputInitQuantsTable

--
findMassExpr :: Expr
findMassExpr = ((sy wMass) $= (sy wVol) * (sy wDensity) $=
  ((sy pi_) * ((((sy diam) / 2) $^ 2)) * (sy tankLength) * (sy wDensity)))

findMass :: ConceptInstance
findMass = findMassConstruct inputInitQuants (phrase mass) (makeRef2S eBalanceOnWtr)
              (E findMassExpr) (ch wVol `isThe` phrase wVol)

--
oIDQQuants :: [Sentence]
oIDQQuants = map foldlSent_ [
  [S "the", plural quantity, S "from", makeRef2S inputInitQuants],
  [S "the", phrase mass, S "from", makeRef2S findMass],
  [ch tauW, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)]
  ]

inputInitQuantsTable :: LabelledContent
inputInitQuantsTable = llcc (makeTabRef "Input-Variable-Requirements") $ 
  Table [titleize symbol_, titleize unit_, titleize description]
  (mkTable [ch, toSentence, phrase] inputVar)
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True

reqs :: [ConceptInstance]
reqs = [inputInitQuants, findMass, checkWithPhysConsts,
        oIDQConstruct oIDQQuants, calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
