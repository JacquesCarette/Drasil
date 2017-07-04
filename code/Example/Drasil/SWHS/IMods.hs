module Drasil.SWHS.IMods where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Prelude hiding (id)
import Drasil.SWHS.Unitals
import Data.Drasil.Utils (getS, unwrap)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, ofThe)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Concepts.Math (equation, change)
import Drasil.SWHS.Concepts
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Thermodynamics
import Control.Lens ((^.))

swhsInModels :: [RelationConcept]
swhsInModels = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP "Energy balance on water to find the temperature of the water")
  balWtrDesc balWtr_Rel
  --FIXME: title uses temp_W title

balWtr_Rel :: Relation
balWtr_Rel = (Deriv Total (C temp_W) (C time)) := (Int 1) / (C tau_W) *
  (((C temp_C) - (FCall (C temp_W) [C time])) +
  (C eta) * ((FCall (C temp_PCM) [C time]) - (FCall (C temp_W) [C time])))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ C temp_W) `isThe` phrase temp_W +:+. sParen (unwrap $ getUnit temp_W),
  (E $ C temp_PCM) `isThe` phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ C temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ C tau_W := (C w_mass * C htCap_W) / (C coil_HTC * C coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  (E $ C eta := (C pcm_HTC * C pcm_SA) / (C coil_HTC * C coil_SA)),
  S "is a constant" +:+. sParen (S "dimensionless"),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ Int 0 :< C temp_W :< (Int 100)),
  sParen (unwrap $ getUnit temp_W), S "where", S $ show (0 :: Integer),
  sParen (unwrap $ getUnit temp_W) `sAnd` (S $ show (100 :: Integer)),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (acroA 14 `sC` acroA 19)]


---------
-- IM2 --
---------
eBalanceOnPCM :: RelationConcept
eBalanceOnPCM = makeRC "eBalanceOnPCM" (nounPhraseSP "Energy balance on PCM to find T_p")
  balPCMDesc balPCM_Rel

balPCM_Rel :: Relation
balPCM_Rel = (Deriv Total (C temp_PCM) (C time)) := Case [case1, case2, case3, case4]
  where case1 = (((Int 1) / (C tau_S_P)) * ((FCall (C temp_W) [C time]) - (FCall (C temp_PCM) [C time])),
          (C temp_PCM) :< (C temp_melt_P))

        case2 = (((Int 1) / (C tau_L_P)) * ((FCall (C temp_W) [C time]) - (FCall (C temp_PCM) [C time])),
          (C temp_PCM) :> (C temp_melt_P))

        case3 = ((Int 0),
          (C temp_PCM) := (C temp_melt_P))

        case4 = ((Int 0),
          (Int 0) :< (C melt_frac) :< (Int 1))


balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ C temp_W) `isThe` phrase temp_W +:+. sParen (unwrap $ getUnit temp_W),
  (E $ C temp_PCM) `isThe` phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (C tau_S_P) := ((C pcm_mass) * (C htCap_S_P)) / ((C pcm_HTC) * (C pcm_SA))),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_S_P),
  (E $ (C tau_L_P) := ((C pcm_mass) * (C htCap_L_P)) / ((C pcm_HTC) * (C pcm_SA))),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_S_P)]


---------
-- IM3 --
---------
heatEInWtr :: RelationConcept
heatEInWtr = makeRC "heatEInWtr" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel

htWtr_Rel :: Relation
htWtr_Rel = (FCall (C w_E) [C time]) := (C htCap_W) * (C w_mass) *
  ((FCall (C temp_W) [C time]) - C temp_init)

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above", phrase equation, S "is derived using" +:+. acroT 2, 
  (getS w_E) `isThe` phrase change, S "in", phrase thermal_energy, S "of the", phrase liquid,
  phrase water, S "relative to the", phrase energy, S "at the initial", phrase temp,
  sParen (getS temp_init) +:+. sParen (unwrap $ getUnit pcm_initMltE), (getS htCap_W) `isThe`
  phrase heat_cap_spec, S "of", phrase liquid, phrase water,
  sParen (unwrap $ getUnit htCap_S_P) `sAnd` (getS w_mass)
  `isThe` phrase mass, S "of the", phrase water +:+. sParen (unwrap $ getUnit w_mass), 
  S "The", phrase change, S "in", phrase temp, S "is the difference between the",
  phrase temp, S "at", phrase time, getS time,
  sParen (unwrap $ getUnit t_init_melt) `sC` (getS temp_W) `sAnd` S "the",
  phrase temp_init `sC` getS temp_init +:+. sParen (unwrap $ getUnit temp_init),
  S "This", phrase equation, S "applies as long as",
  (E $ (Int 0) :< (C temp_W) :< (Int 0)) :+: (unwrap $ getUnit temp_W),
  sParen (acroA 14 `sC` acroA 19)]

---------
-- IM4 --
---------
heatEInPCM :: RelationConcept
heatEInPCM = makeRC "heatEInPCM" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCM_Rel

htPCM_Rel :: Relation
htPCM_Rel = C pcm_E := Case [case1, case2, case3, case4]
  where case1 = (C htCap_S_P * C pcm_mass * ((FCall (C temp_PCM) [C time]) - C temp_init),
          (C temp_PCM) :< (C temp_melt_P))

        case2 = (C pcm_initMltE + (C htFusion * C pcm_mass) + (C htCap_L_P * C pcm_mass * ((FCall (C temp_PCM) [C time]) - C temp_melt_P)),
          (C temp_PCM) :> (C temp_melt_P))

        case3 = (C pcm_initMltE + (FCall (C latentE_P) [C time]),
          (C temp_PCM) := (C temp_melt_P))

        case4 = (C pcm_initMltE + (FCall (C latentE_P) [C time]),
          (Int 0) :< (C melt_frac) :< (Int 1))

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above", phrase equation,
  S "is derived using" +:+. (acroT 2 `sAnd` acroT 3), getS pcm_E `isThe`
  phrase change, S "in", phrase thermal_energy, S "of the", short phsChgMtrl,
  S "relative to the", phrase energy, S "at the", phrase temp_init,
  sParen (getS temp_init) +:+. (unwrap $ getUnit pcm_initMltE), getS pcm_E,
  S "for the", phrase solid, short phsChgMtrl, S "is found using", acroT 2,
  S "for", phrase sens_heat, S "ing, with", phrase heat_cap_spec `ofThe`
  phrase solid, short phsChgMtrl `sC` getS htCap_S_P,
  sParen (unwrap $ getUnit htCap_S_P), S "and the", phrase change, S "in the",
  short phsChgMtrl, phrase temp, S "from the", phrase temp_init +:+.
  sParen (unwrap $ getUnit temp_init), getS pcm_E, S "for the melted",
  short phsChgMtrl, sParen (E (C temp_PCM :> C pcm_initMltE)),
  S "is found using", acroT 2, S "for", phrase sens_heat, S "of the" +:+.
  phrase liquid, short phsChgMtrl, S "plus the", phrase energy, S "when",
  phrase melting, S "starts, plus the", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "The", phrase energy,
  S "when", phrase melting, S "starts is", getS pcm_initMltE +:+.
  sParen (unwrap $ getUnit pcm_initMltE), S "The", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "is",
  E (C htFusion :* C pcm_mass), sParen (unwrap $ getUnit pcm_initMltE) +:+.
  sParen (acroDD 3), phrase heat_cap_spec `ofThe` phrase liquid,
  short phsChgMtrl, S "is", getS htCap_L_P,
  sParen (unwrap $ getUnit htCap_L_P) `sAnd` S "the", phrase change, S "in",
  phrase temp, S "is", E (C temp_PCM :- C temp_melt_P) +:+.
  sParen (unwrap $ getUnit temp_melt_P), getS pcm_E, S "during",
  phrase melting, S "of the", short phsChgMtrl, S "is found using the", 
  phrase energy, S "required at", S "instant" +:+ phrase melting `ofThe`
  short phsChgMtrl, S "begins" `sC` getS pcm_initMltE, S "plus the",
  phrase latent_heat, phrase energy, S "added to the", short phsChgMtrl `sC`
  getS latentE_P, sParen (unwrap $ getUnit latentE_P), S "since the",
  phrase time, S "when", phrase melting, S "began", getS t_init_melt +:+.
  sParen (unwrap $ getUnit t_init_melt), S "The", phrase heat, phrase energy,
  S "for", phrase boiling, S "of the", short phsChgMtrl,
  S "is not detailed" `sC` S "since the", short phsChgMtrl,
  S "is assumed to either be in a", phrase solid, S "or", phrase liquid,
  S "state", sParen (acroA 18)]



{--varWithDesc :: N c => c -> Sentence
varWithDesc conceptVar = (E $ C conceptVar) `isThe` phrase conceptVar +:+.
  sParen (unwrap $ getUnit conceptVar)

--need to create a wrapper
--}
