module Drasil.SWHS.IMods (swhsIMods,
  eBalanceOnWtr, heatEInWtr) where

import Language.Drasil

import Drasil.SWHS.Unitals (t_init_melt, latentE_P, pcm_E, pcm_initMltE,
  temp_melt_P, temp_PCM, htCap_L_P, pcm_mass, htFusion, temp_init, htCap_S_P,
  melt_frac, temp_W, w_mass, w_E, htCap_W, tau_S_P, pcm_SA, tau_L_P, pcm_HTC,
  coil_SA, coil_HTC, eta, tau_W, temp_C)
import Data.Drasil.Utils (getES, unwrap)
import Data.Drasil.SentenceStructures (acroT, foldlSent, isThe,
  sAnd, ofThe)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Concepts.Math (equation, change)
import Drasil.SWHS.Concepts (phsChgMtrl, water)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, mass)
import Data.Drasil.Concepts.Thermodynamics (boiling, heat, temp, melting,
  latent_heat, sens_heat, heat_cap_spec, thermal_energy, boil_pt)
import Drasil.SWHS.DataDefs (ddRef, dd3HtFusion)
import Drasil.SWHS.Labels (assump14Label, assump19Label, assump18Label)

swhsIMods :: [RelationConcept]
swhsIMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel Nothing--label

balWtr_Rel :: Relation
balWtr_Rel = (deriv (sy temp_W) time) $= 1 / (sy tau_W) *
  (((sy temp_C) - (apply1 temp_W time)) +
  (sy eta) * ((apply1 temp_PCM time) - (apply1 temp_W time)))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  (E $ sy eta $= (sy pcm_HTC * sy pcm_SA) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (S "dimensionless"),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100))),
  sParen (unwrap $ getUnit temp_W), S "where", E 0,
  sParen (unwrap $ getUnit temp_W) `sAnd` (E 100),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef assump14Label `sC` makeRef assump19Label)]


---------
-- IM2 --
---------
eBalanceOnPCM :: RelationConcept
eBalanceOnPCM = makeRC "eBalanceOnPCM" (nounPhraseSP
  "Energy balance on PCM to find T_p")
  --FIXME: T_p should be called from symbol
  balPCMDesc balPCM_Rel Nothing--label

balPCM_Rel :: Relation
balPCM_Rel = (deriv (sy temp_PCM) time) $= case_ [case1, case2, case3, case4]
  where case1 = ((1 / (sy tau_S_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpTo (Exc,sy temp_melt_P)))

        case2 = ((1 / (sy tau_L_P)) * ((apply1 temp_W time) -
          (apply1 temp_PCM time)), real_interval temp_PCM (UpFrom (Exc,sy temp_melt_P)))

        case3 = (0, (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (0, real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

balPCMDesc :: Sentence
balPCMDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), (E $ sy temp_PCM) `isThe`
  phrase temp_PCM +:+. sParen (unwrap $ getUnit temp_PCM),
  (E $ (sy tau_S_P) $= ((sy pcm_mass) * (sy htCap_S_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))), S "is a constant" +:+.
  sParen (unwrap $ getUnit tau_S_P), (E $ (sy tau_L_P) $=
  ((sy pcm_mass) * (sy htCap_L_P)) / ((sy pcm_HTC) * (sy pcm_SA))),
  S "is a constant", sParen (unwrap $ getUnit tau_S_P)]


---------
-- IM3 --
---------
heatEInWtr :: RelationConcept
heatEInWtr = makeRC "heatEInWtr" (nounPhraseSP "Heat energy in the water")
  htWtrDesc htWtr_Rel Nothing--label

htWtr_Rel :: Relation
htWtr_Rel = (apply1 w_E time) $= (sy htCap_W) * (sy w_mass) *
  ((apply1 temp_W time) - sy temp_init)

htWtrDesc :: Sentence
htWtrDesc = foldlSent [S "The above", phrase equation,
  S "is derived using" +:+. acroT 2, getES w_E `isThe` phrase change,
  S "in", phrase thermal_energy, S "of the", phrase liquid,
  phrase water, S "relative to the", phrase energy, S "at the initial",
  phrase temp, sParen (getES temp_init) +:+.
  sParen (unwrap $ getUnit pcm_initMltE), (getES htCap_W) `isThe`
  phrase heat_cap_spec, S "of", phrase liquid, phrase water,
  sParen (unwrap $ getUnit htCap_S_P) `sAnd` (getES w_mass)
  `isThe` phrase mass, S "of the", phrase water +:+.
  sParen (unwrap $ getUnit w_mass), S "The", phrase change, S "in",
  phrase temp, S "is the difference between the", phrase temp, S "at",
  phrase time, getES time, sParen (unwrap $ getUnit t_init_melt) `sC`
  (getES temp_W) `sAnd` S "the", phrase temp_init `sC` getES temp_init +:+.
  sParen (unwrap $ getUnit temp_init), S "This", phrase equation,
  S "applies as long as", (E $ real_interval temp_W (Bounded (Exc,0) (Exc,100)))
  :+: (unwrap $ getUnit temp_W),
  sParen $ makeRef assump14Label `sC` makeRef assump19Label]

---------
-- IM4 --
---------
heatEInPCM :: RelationConcept
heatEInPCM = makeRC "heatEInPCM" (nounPhraseSP "Heat energy in the PCM")
  htPCMDesc htPCM_Rel Nothing--label

htPCM_Rel :: Relation
htPCM_Rel = sy pcm_E $= case_ [case1, case2, case3, case4]
  where case1 = (sy htCap_S_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_init), real_interval temp_PCM (UpTo (Exc, sy temp_melt_P)))

        case2 = (sy pcm_initMltE + (sy htFusion * sy pcm_mass) +
          (sy htCap_L_P * sy pcm_mass * ((apply1 temp_PCM time) -
          sy temp_melt_P)), real_interval temp_PCM (UpFrom (Exc, sy temp_melt_P)))

        case3 = (sy pcm_initMltE + (apply1 latentE_P time),
          (sy temp_PCM) $= (sy temp_melt_P))

        case4 = (sy pcm_initMltE + (apply1 latentE_P time),
          real_interval melt_frac (Bounded (Exc,0) (Exc,1)))

htPCMDesc :: Sentence
htPCMDesc = foldlSent [S "The above", phrase equation,
  S "is derived using" +:+. (acroT 2 `sAnd` acroT 3), getES pcm_E `isThe`
  phrase change, S "in", phrase thermal_energy, S "of the", short phsChgMtrl,
  S "relative to the", phrase energy, S "at the", phrase temp_init,
  sParen (getES temp_init) +:+. (unwrap $ getUnit pcm_initMltE), getES pcm_E,
  S "for the", phrase solid, short phsChgMtrl, S "is found using", acroT 2,
  S "for", phrase sens_heat, S "ing, with", phrase heat_cap_spec `ofThe`
  phrase solid, short phsChgMtrl `sC` getES htCap_S_P,
  sParen (unwrap $ getUnit htCap_S_P), S "and the", phrase change, S "in the",
  short phsChgMtrl, phrase temp, S "from the", phrase temp_init +:+.
  sParen (unwrap $ getUnit temp_init), getES pcm_E, S "for the melted",
  short phsChgMtrl, sParen (E (sy temp_PCM $> sy pcm_initMltE)),
  S "is found using", acroT 2, S "for", phrase sens_heat, S "of the" +:+.
  phrase liquid, short phsChgMtrl, S "plus the", phrase energy, S "when",
  phrase melting, S "starts, plus the", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "The", phrase energy,
  S "when", phrase melting, S "starts is", getES pcm_initMltE +:+.
  sParen (unwrap $ getUnit pcm_initMltE), S "The", phrase energy,
  S "required to melt all of the", short phsChgMtrl, S "is",
  E (sy htFusion * sy pcm_mass), sParen (unwrap $ getUnit pcm_initMltE) +:+.
  sParen (ddRef dd3HtFusion), phrase heat_cap_spec `ofThe` phrase liquid,
  short phsChgMtrl, S "is", getES htCap_L_P,
  sParen (unwrap $ getUnit htCap_L_P) `sAnd` S "the", phrase change, S "in",
  phrase temp, S "is", E (sy temp_PCM - sy temp_melt_P) +:+.
  sParen (unwrap $ getUnit temp_melt_P), getES pcm_E, S "during",
  phrase melting, S "of the", short phsChgMtrl, S "is found using the", 
  phrase energy, S "required at", S "instant" +:+ phrase melting `ofThe`
  short phsChgMtrl, S "begins" `sC` getES pcm_initMltE, S "plus the",
  phrase latent_heat, phrase energy, S "added to the", short phsChgMtrl `sC`
  getES latentE_P, sParen (unwrap $ getUnit latentE_P), S "since the",
  phrase time, S "when", phrase melting, S "began", getES t_init_melt +:+.
  sParen (unwrap $ getUnit t_init_melt), S "The", phrase heat, phrase energy,
  S "for", phrase boiling, S "of the", short phsChgMtrl,
  S "is not detailed" `sC` S "since the", short phsChgMtrl,
  S "is assumed to either be in a", phrase solid, S "or", phrase liquid,
  S "state", sParen (makeRef assump18Label)]