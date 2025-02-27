module Drasil.SWHSNoPCM.Requirements (funcReqs, inReqDesc) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.DocLang (inReq)
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
findMass :: ConceptInstance
findMass = findMassConstruct (inReq EmptyS) (phrase mass) [eBalanceOnWtr]
            [waterMass, waterVolume, tankVolume]

--
oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [pluralNP (the value), fromSource (inReq EmptyS)],
  [phraseNP (the mass), fromSource findMass],
  [ch (balanceDecayRate ^. defLhs), fromSource balanceDecayRate]
  ]

funcReqs :: [ConceptInstance]
funcReqs = [findMass, checkWithPhysConsts, oIDQConstruct oIDQVals, 
            calcValues noPCMOutputs, outputValues noPCMOutputs]

noPCMOutputs :: [InstanceModel]
noPCMOutputs = [eBalanceOnWtr, heatEInWtr]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS
