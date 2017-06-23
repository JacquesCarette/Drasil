module Drasil.SWHS.IMods where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Prelude hiding (id)
import Drasil.SWHS.Unitals
import Data.Drasil.Utils (getS, unwrap)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, ofThe)
import Data.Drasil.Quantities.Physics (time)
import Control.Lens ((^.))

iModels :: [RelationConcept]
iModels = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

{-IM1-}

eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP "Energy balance on water to find T_w") --FIXME:title's "T_w"
  balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = Int 0 := Int 0

balWtrDesc :: Sentence
balWtrDesc = fixmeS

{-IM2-}

eBalanceOnPCM :: RelationConcept
eBalanceOnPCM = makeRC "eBalanceOnPCM" (nounPhraseSP "Energy balance on PCM to find T_p")
  balPCMDesc balPCM_Rel

balPCM_Rel :: Relation
balPCM_Rel = Int 0 := Int 0

balPCMDesc :: Sentence
balPCMDesc = fixmeS

{-IM3-}

heatEInWtr :: RelationConcept
heatEInWtr = makeRC "heatEInWtr" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel

htWtr_Rel :: Relation
htWtr_Rel = Int 0 := Int 0

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above equation is derived using" +:+. acroT "2", 
  (getS w_E) `isThe` S "change in thermal energy of the liquid water relative to",
  S "the energy at the initial temperature", sParen (getS temp_init) +:+.
  sParen (unwrap $ getUnit pcm_initMltE), (getS htCap_W) `isThe` S "specific heat",
  S "capacity of liquid water", (((sParen (unwrap $ getUnit htCap_S_P)) `sAnd` (getS w_mass))
  `isThe` S "mass of the water") +:+. sParen (unwrap $ getUnit w_mass), 
  S "The change in temperature is the difference between the temperature at time", 
  P (time ^. symbol), sParen (unwrap $ getUnit t_init_melt) `sC` (getS temp_W) `sAnd`
  S "the", phrase temp_init `sC` getS temp_init +:+. sParen (unwrap $ getUnit temp_init),
  S "This equation applies as long as", E ((Int 0) :< (C temp_W) :< (Int 0)) :+: 
  (unwrap $ getUnit temp_W), sParen (acroA "14" `sC` acroA "19")]

{-IM4-}

heatEInPCM :: RelationConcept
heatEInPCM = makeRC "heatEInPCM" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCM_Rel

htPCM_Rel :: Relation
htPCM_Rel = Int 0 := Int 0

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above equation is derived using" +:+. (acroT "2" `sAnd` acroT "3"),
  (getS pcm_E) `isThe` S "change in thermal energy of the PCM relative to the energy",
  S "at the", (phrase temp_init), sParen (getS temp_init) +:+. (unwrap $ getUnit pcm_initMltE), (getS pcm_E), 
  S "for the solid PCM is found using", acroT "2", S "for sensible heating, with", S "specific heat capacity" `ofThe`
  S "solid PCM" `sC` (getS htCap_S_P), sParen (unwrap $ getUnit htCap_S_P), S "and the change in the PCM temperature", 
  S "from the", (phrase temp_init) +:+. sParen (unwrap $ getUnit temp_init), (getS pcm_E), S "for the melted PCM",
  sParen (E ((C temp_PCM) :> (C pcm_initMltE))), S "is found using", acroT "2", S "for sensible heating of the" +:+. 
  S "liquid PCM, plus the energy when melting starts, plus the energy required to melt all of the PCM",
  S "The energy when melting starts is", (getS pcm_initMltE) +:+. sParen (unwrap $ getUnit pcm_initMltE),
  S "The energy required to melt all of the PCM is", E ((C htFusion) :* (C pcm_mass)), 
  sParen (unwrap $ getUnit pcm_initMltE) +:+. sParen (acroDD "3"),
  S "The specific heat capacity of the liquid PCM is", (getS htCap_L_P),
  sParen (unwrap $ getUnit htCap_L_P) `sAnd` S "the change in temperature is", E ((C temp_PCM) :- (C temp_melt_P)) +:+.
  sParen (unwrap $ getUnit temp_melt_P), (getS pcm_E), S "during melting of the PCM is found using the", 
  S "energy required at the instant melting of the PCM begins" `sC` (getS pcm_initMltE), 
  S "plus the latent heat energy added to the PCM, QP (J) since the time when melting began", 
  (getS t_init_melt) +:+. sParen (unwrap $ getUnit t_init_melt), S "The heat energy for boiling of the PCM is not detailed" `sC`
  S "since the PCM is assumed to either be in a solid or liquid state", sParen (acroA "18")]

{--}

fixmeS :: Sentence
fixmeS = S "Add description"