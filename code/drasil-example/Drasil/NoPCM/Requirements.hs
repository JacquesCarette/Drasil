module Drasil.NoPCM.Requirements (funcReqsList, reqs, dataConstListIn) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation as Doc (description, input_,
  quantity, requirement, symbol_, variable)

import Data.Drasil.Concepts.Math (unit_)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.DocLang (mkEnumSimpleD)
import Drasil.DocumentLanguage.Units (toSentence) 
import Data.Drasil.SentenceStructures (foldlSent_, isThe)

import Drasil.SWHS.Requirements (calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime,
  checkWithPhysConsts, findMassConstruct, iIQConstruct, oIDQConstruct)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, diam, htCap_W, tank_length,
  tau_W, temp_C, time_final, w_density, w_mass, w_vol)

import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.NoPCM.Unitals (temp_init)

inputVar :: [QuantityDict]
inputVar = map qw dataConstListIn 

dataConstListIn :: [UncertQ]
dataConstListIn = [tank_length, diam, coil_SA, temp_C, w_density, htCap_W,
  coil_HTC, temp_init, time_final]

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
findMassExpr = ((sy w_mass) $= (sy w_vol) * (sy w_density) $=
  ((((sy diam) / 2) $^ 2) * (sy tank_length) * (sy w_density)))

findMass :: ConceptInstance
findMass = findMassConstruct inputInitQuants (phrase mass) (makeRef2S eBalanceOnWtr)
              (E findMassExpr) (ch w_vol `isThe` phrase w_vol)

--
oIDQQuants :: [Sentence]
oIDQQuants = map foldlSent_ [
  [S "the", plural quantity, S "from", makeRef2S inputInitQuants],
  [S "the", phrase mass, S "from", makeRef2S findMass],
  [ch tau_W, sParen (S "from" +:+ makeRef2S eBalanceOnWtr)]
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
