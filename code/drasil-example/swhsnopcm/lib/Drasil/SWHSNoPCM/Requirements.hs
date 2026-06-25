module Drasil.SWHSNoPCM.Requirements (
  funcReqs, inReqDesc, funcReqsTables
) where

import Control.Lens ((^.))
import qualified Data.List.NonEmpty as NE (fromList)

import Language.Drasil
import Language.Drasil.Document
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.SRS (inReqWTab, outReq, mkQRTuple)

import Theory.Drasil (InstanceModel, HasOutput(output))

import Data.Drasil.Concepts.Documentation (value, funcReqDom)
import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.SWHS.DataDefs (waterMass, tankVolume, balanceDecayRate)
import Drasil.SWHS.IMods (heatEInWtr)
import Drasil.SWHS.Requirements (checkWithPhysConsts,
  findMassConstruct, inReqDesc, oIDQConstruct, time)

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
            calcValues, outputValues]

outputValues, calcValues :: ConceptInstance

calcValues = cic "calcValues" (S "Calculate the following" +: plural value +:+.
  foldlList Comma List (map (\x -> ch (x ^. output) :+: sParen (ch time) +:+ fromSource x) noPCMOutputs))
  "Calculate-Values" funcReqDom

(outputValues, _) = outReq (Just $ S "over" +:+ ch time) (NE.fromList $ mkQRTuple noPCMOutputs)

noPCMOutputs :: [InstanceModel]
noPCMOutputs = [eBalanceOnWtr, heatEInWtr]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
