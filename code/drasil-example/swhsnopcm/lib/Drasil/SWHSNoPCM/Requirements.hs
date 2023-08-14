module Drasil.SWHSNoPCM.Requirements (funcReqs, inputInitValsTable) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.DocLang (mkInputPropsTable)
import Language.Drasil.Chunk.Concept.NamedCombinators
import Theory.Drasil (InstanceModel)

import Data.Drasil.Concepts.Documentation (funcReqDom, input_, value)
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

--
inputInitVals :: ConceptInstance
inputInitVals = cic "inputInitVals" (foldlSent [
  titleize input_, S "the following", plural value, S "described in the table for",
  namedRef inputInitValsTable (S "Required Inputs")`sC` S "which define", inReqDesc])
  "Input-Initial-Values" funcReqDom

--
findMass :: ConceptInstance
findMass = findMassConstruct inputInitVals (phrase mass) [eBalanceOnWtr]
            [waterMass, waterVolume, tankVolume]

--
oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [pluralNP (the value), fromSource inputInitVals],
  [phraseNP (the mass), fromSource findMass],
  [ch (balanceDecayRate ^. defLhs), fromSource balanceDecayRate]
  ]

inputInitValsTable :: LabelledContent
inputInitValsTable = mkInputPropsTable inputs inputInitVals

funcReqs :: [ConceptInstance]
funcReqs = [inputInitVals, findMass, checkWithPhysConsts,
  oIDQConstruct oIDQVals, calcValues noPCMOutputs, outputValues noPCMOutputs]

noPCMOutputs :: [InstanceModel]
noPCMOutputs = [eBalanceOnWtr, heatEInWtr]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
