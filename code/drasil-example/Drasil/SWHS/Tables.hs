module Drasil.SWHS.Tables where --all of this file is exported

import Language.Drasil
import qualified Drasil.DocumentLanguage.Units as U (toSentence)
import Drasil.SWHS.Labels (inputInitQuantsLbl)
import Data.Drasil.Concepts.Documentation (symbol_, description, input_, variable, requirement)
import Data.Drasil.Concepts.Math (unit_)
import Drasil.SWHS.Unitals (pcm_SA, pcm_HTC, 
  temp_C, coil_SA,coil_HTC, htCap_S_P, htCap_W,
  time_final, w_density, temp_init, htCap_L_P, htFusion, pcm_density,
  temp_melt_P, pcm_vol, diam, tank_length, abs_tol, rel_tol, cons_tol)

inputInitQuantsTbl :: Contents
inputInitQuantsTbl = LlC inputInitQuantsTblabled

inputInitQuantsTblabled :: LabelledContent
inputInitQuantsTblabled = llcc inputInitQuantsLbl $ (Table
  [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch, --(\ch -> Sy (unit_symb ch)),
  U.toSentence, phrase] ((map qw inputConstraints) ++ (map qw [abs_tol, rel_tol, cons_tol])))
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True)

inputConstraints :: [UncertQ]
inputConstraints = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA,
  temp_C, w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]
