module Drasil.SWHS.LikelyChanges where

import Language.Drasil

import Drasil.DocumentLanguage (mkLklyChnk)
import Data.Drasil.Concepts.Documentation (assumption, value, simulation,
  model)

import Drasil.SWHS.Concepts (tank, phsChgMtrl, water)
import Drasil.SWHS.Unitals (temp_init, temp_C, temp_PCM)
import Drasil.SWHS.Assumptions (assump4, assump8, assump9, assump11,
  assump12, assump15)

import Data.Drasil.Concepts.Thermodynamics as CT (heat,
  thermal_conductor)
import Data.Drasil.Quantities.Physics (energy)

import Data.Drasil.SentenceStructures (foldlSent, sAnd, ofThe)

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

s6_start :: Contents -> Sentence
s6_start a = makeRef a +:+ S "-"

likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6 :: Contents

likeChg1 = mkLklyChnk "likeChg1" (
  foldlSent [s6_start assump4, short phsChgMtrl, S "is actually a poor", 
  phrase CT.thermal_conductor `sC` S "so the", phrase assumption, 
  S "of uniform", phrase temp_PCM, S "is not likely"] ) (S "assumChg")
--
likeChg2 = mkLklyChnk "likeChg2" (
  foldlSent [s6_start assump8, S "The", phrase temp_C, S "will change over", 
  (S "course" `ofThe` S "day, depending"), S "on the", phrase energy, 
  S "received from the sun"] ) (S "tempCoilTimeChg")
--
likeChg3 = mkLklyChnk "likeChg3" (
  foldlSent [s6_start assump9, S "The", phrase temp_C,
  S "will actually change along its length as the", phrase water,
  S "within it cools"] ) (S "tempCoilLengthChg")
--
likeChg4 = mkLklyChnk "likeChg4" (
  foldlSent [s6_start assump11, S "The", phrase model, S "currently only", 
  S "accounts for charging of the tank. A more complete", phrase model, 
  S "would also account for discharging of the tank"] ) (S "dischargeChg")
--
likeChg5 = mkLklyChnk "likeChg5" (
  foldlSent [s6_start assump12, S "To add more flexibility to the", 
  phrase simulation `sC` (phrase temp_init `ofThe` phrase water) `sAnd`
  S "the", short phsChgMtrl, S "could be allowed to have different", 
  plural value] ) (S "diffTempsChg")
--
likeChg6 = mkLklyChnk "likeChg6" (
  foldlSent [s6_start assump15, S "Any real", phrase tank, S "cannot", 
  S "be perfectly insulated and will lose", phrase CT.heat] ) (S "loseHeatChg")

-- List structure same in all examples.

--add referencing to assumptions?
