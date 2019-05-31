module Drasil.SWHS.Tables where --all of this file is exported

import Language.Drasil
import qualified Drasil.DocumentLanguage.Units as U (toSentence)
import Drasil.SWHS.Labels (inputInitQuantsLbl)
import Data.Drasil.Concepts.Documentation (symbol_, description, input_, variable, requirement)
import Data.Drasil.Concepts.Math (unit_)
import Drasil.SWHS.Unitals (pcmSA, pcm_HTC, 
  tempC, coilSA,coilHTC, htCapSP, htCapW,
  time_final, wDensity, tempInit, htCapLP, htFusion, pcmDensity,
  tempMeltP, pcmVol, diam, tankLength, abs_tol, rel_tol, cons_tol)

inputInitQuantsTbl :: Contents
inputInitQuantsTbl = LlC inputInitQuantsTblabled

inputInitQuantsTblabled :: LabelledContent
inputInitQuantsTblabled = llcc inputInitQuantsLbl (Table
  [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch, --(\ch -> Sy (unit_symb ch)),
  U.toSentence, phrase] ((map qw inputConstraints) ++ (map qw [abs_tol, rel_tol, cons_tol])))
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True)

inputConstraints :: [UncertQ]
inputConstraints = [tankLength, diam, pcmVol, pcmSA, pcmDensity,
  tempMeltP, htCapSP, htCapLP, htFusion, coilSA,
  tempC, wDensity, htCapW, coilHTC, pcm_HTC, tempInit, time_final]
