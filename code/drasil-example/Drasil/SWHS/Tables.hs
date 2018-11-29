module Drasil.SWHS.Tables where --all of this file is exorted

import Language.Drasil
import qualified Drasil.DocumentLanguage.Units as U (toSentence)
import Drasil.SWHS.Labels (inputInitQuantsLbl)
import Data.Drasil.Concepts.Documentation (symbol_, description, input_, variable, requirement)
import Data.Drasil.Concepts.Math (unit_)
import Drasil.SWHS.Unitals (pcm_SA, temp_W, temp_PCM, pcm_HTC, pcm_E,
  temp_C, coil_SA, w_E, coil_HTC, sim_time, tau_S_P, htCap_S_P, pcm_mass,
  ht_flux_P, eta, tau_W, htCap_W, w_mass, ht_flux_C, vol_ht_gen, thickness,
  out_SA, ht_flux_out, ht_flux_in, in_SA, thFluxVect, time_final,
  specParamValList, w_density, temp_init, htCap_L_P, htFusion, pcm_density,
  temp_melt_P, pcm_vol, diam, tank_length, swhsConstrained, swhsOutputs, 
  swhsInputs, swhsSymbols, swhsSymbolsAll, swhsUC)


inputInitQuantsTbl :: Contents
inputInitQuantsTbl = LlC $ llcc inputInitQuantsLbl $ (Table
  [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch, --(\ch -> Sy (unit_symb ch)),
  U.toSentence, phrase] (map qw inputConstraints))
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True)

inputInitQuantsTblabled :: LabelledContent
inputInitQuantsTblabled = llcc inputInitQuantsLbl $ (Table
  [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch, --(\ch -> Sy (unit_symb ch)),
  U.toSentence, phrase] (map qw inputConstraints))
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True)

inputConstraints :: [UncertQ]
inputConstraints = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA,
  temp_C, w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]