module Drasil.SWHS.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Software (program)
import Control.Lens ((^.))

---Acronyms---
phsChgMtrl,rightSide,progName :: CINP
--FIXME: Use compound nounphrases instead of pn/cn
phsChgMtrl  = commonINP "phsChgMtrl" (pn' "Phase Change Material")      "PCM"
rightSide   = commonINP "rightSide"  (cn' "Right Hand Side")            "RHS" 
progName    = commonINP "progName"   (nounPhrase "solar water heating system" "solar water heating systems") "SWHS" 

swhsFull :: NPNC
swhsFull    = npnc "swhsFull" (progName `with'` phsChgMtrl)
-- I want to include SI as an acronym, but I can't find a way for the 
-- description to have accents when using dcc.

---ConceptChunks---

charging, coil, discharging, gauss_div,
  perfect_insul, phase_change_material, swhs_pcm, tank,
  tank_pcm, transient, water :: ConceptChunk
swhsProg :: NPNC
--FIXME: There are too many "swhs" chunks for very minor differences.
  
charging = dcc "charging" (nounPhraseSP "charging") "charging of the tank"
coil = dcc "coil" (cn' "heating coil")
  "Coil in tank that heats by absorbing solar energy"
discharging = dcc "discharging" (nounPhraseSP "discharging") 
  "Discharging of the tank"
gauss_div = dcc "gauss_div" (nounPhraseSP "gauss's divergence theorem")
  "A result that relates the flow of a vector field through a surface to the behavior of the vector field inside the surface"
--TODO: Physical property.
perfect_insul = dcc "perfect_insul" (nounPhraseSP "perfectly insulated")
  "Describes the property of a material not allowing heat transfer through its boundaries"           
phase_change_material = dcc "pcm" (nounPhraseSP "Phase Change Material")
      ("A substance that uses phase changes (such as melting) to absorb or " ++
      "release large amounts of heat at a constant temperature")
      
--FIXME: Temporarily have to manually create the compound phrase, because it
--uses acronym and a sentence.

--Didn't work
--swhsProg = npnc' "swhsProg" (nounPhrase''
--  (compoundNPNC'' short phrase progName (program ^. term))
--  (short progName +:+ (phrase $ program ^. term))
--  CapFirst CapWords) "SWHS"

swhsProg = npnc' "swhsProg" (nounPhrase'' (short progName +:+ 
  (phrase $ program ^. term)) (short progName +:+ (phrase $ program ^. term))
  CapFirst CapWords) "SWHS"

swhs_pcm = dcc "swhs_pcm" (nounPhraseSP 
  "solar water heating systems incorporating PCM")
  "Solar water heating systems incorporating phase change material"
tank = dcc "tank" (cn' "tank") "solar water heating tank"
tank_pcm = dcc "tank_pcm" 
  (nounPhraseSP "solar water heating tank incorporating PCM")
  "Solar water heating tank incorporating phase change material"


transient = dcc "transient" (nounPhraseSP "transient") "Changing with time"
water = dcc "water" (cn' "water") "The liquid with which the tank is filled"
