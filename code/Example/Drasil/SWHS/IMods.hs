module Drasil.SWHS.IMods where

import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Prelude hiding (id, exp)

iModels :: [RelationConcept]
iModels = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

{-IM1-}

eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP "Energy balance on water to find T_w") --FIXME:title's "T_w"
  balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = Int 0

balWtrDesc :: Sentence
balWtrDesc = fixmeS

{-IM2-}

eBalanceOnPCM :: RelationConcept
eBalanceOnPCM = makeRC "eBalanceOnPCM" (nounPhraseSP "Energy balance on PCM to find T_p")
  balPCMDesc balPCM_Rel

balPCM_Rel :: Relation
balPCM_Rel = Int 0

balPCMDesc :: Sentence
balPCMDesc = fixmeS

{-IM3-}

heatEInWtr :: RelationConcept
heatEInWtr = makeRC "heatEInWtr" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel

htWtr_Rel :: Relation
htWtr_Rel = Int 0

htWtrDesc :: Sentence
htWtrDesc = fixmeS

{-IM4-}

heatEInPCM :: RelationConcept
heatEInPCM = makeRC "heatEInPCM" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCM_Rel

htPCM_Rel :: Relation
htPCM_Rel = Int 0

htPCMDesc :: Sentence
htPCMDesc = fixmeS

{--}

fixmeS :: Sentence
fixmeS = S "Add description"