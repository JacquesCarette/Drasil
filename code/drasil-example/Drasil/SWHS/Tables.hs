module Drasil.SWHS.Tables where --all of this file is exported

import Language.Drasil
import qualified Drasil.DocumentLanguage.Units as U (toSentence)
import Drasil.SWHS.Labels (inputInitQuantsLbl)
import Data.Drasil.Concepts.Documentation (symbol_, description, input_, variable, requirement)
import Data.Drasil.Concepts.Math (unit_)
import Drasil.SWHS.Unitals (pcmSA, pcmHTC, 
  tempC, coilSA,coilHTC, htCapSP, htCapW,
  timeFinal, wDensity, tempInit, htCapLP, htFusion, pcmDensity,
  tempMeltP, pcmVol, diam, tankLength, absTol, relTol, cons_tol)

inputInitQuantsTbl :: Contents
inputInitQuantsTbl = LlC inputInitQuantsTblabled

inputInitQuantsTblabled :: LabelledContent
inputInitQuantsTblabled = llcc inputInitQuantsLbl (Table
  [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch, --(\ch -> Sy (unit_symb ch)),
  U.toSentence, phrase] ((map qw inputConstraints) ++ (map qw [absTol, relTol, cons_tol])))
  (titleize input_ +:+ titleize variable +:+ titleize' requirement) True)

inputConstraints :: [UncertQ]
inputConstraints = [tankLength, diam, pcmVol, pcmSA, pcmDensity,
  tempMeltP, htCapSP, htCapLP, htFusion, coilSA,
  tempC, wDensity, htCapW, coilHTC, pcmHTC, tempInit, timeFinal]
