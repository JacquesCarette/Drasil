module Drasil.SWHSNoPCM.Requirements (
  funcReqs, inReqDesc, funcReqsTables
) where

import Control.Lens ((^.))

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.DocLang (inReqWTab)

import Theory.Drasil (InstanceModel)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.SWHS.DataDefs (waterMass, tankVolume, balanceDecayRate)
import Drasil.SWHS.IMods (heatEInWtr)
import Drasil.SWHS.Requirements (calcValues, checkWithPhysConsts,
  findMassConstruct, inReqDesc, oIDQConstruct, outputValues)

import Drasil.SWHSNoPCM.DataDefs (waterVolume)
import Drasil.SWHSNoPCM.IMods (eBalanceOnWtr)
import Drasil.SWHSNoPCM.Unitals (inputs)

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

funcReqsTables :: [LabelledContent]
funcReqsTables = [inputValuesTable]

inputValues :: ConceptInstance
inputValuesTable :: LabelledContent
(inputValues, inputValuesTable) = inReqWTab (Just inReqDesc) inputs

--
findMass :: ConceptInstance
findMass = findMassConstruct inputValues (phrase mass) [eBalanceOnWtr]
            [waterMass, waterVolume, tankVolume]

--
oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [D.toSent (pluralNP (the value)), fromSource inputValues],
  [D.toSent (phraseNP (the mass)), fromSource findMass],
  [ch (balanceDecayRate ^. defLhs), fromSource balanceDecayRate]
  ]

funcReqs :: [ConceptInstance]
funcReqs = [inputValues, findMass, checkWithPhysConsts, oIDQConstruct oIDQVals,
            calcValues noPCMOutputs, outputValues noPCMOutputs]

noPCMOutputs :: [InstanceModel]
noPCMOutputs = [eBalanceOnWtr, heatEInWtr]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
