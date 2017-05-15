module Drasil.SWHS.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Software (program)
import Control.Lens ((^.))
--- convenience
fixme :: String
fixme = "FIXME: Define this"
---Acronyms---
phsChgMtrl,rightSide,progName :: CINP
--FIXME: Use compound nounphrases instead of pn/cn
phsChgMtrl  = commonINP "phsChgMtrl" (pn' "Phase Change Material")      "PCM"
rightSide   = commonINP "rightSide"  (cn' "Right Hand Side")            "RHS" 
progName    = commonINP "progName"   (pn' "solar water heating system") "SWHS" 

swhsFull :: NPNC
swhsFull    = npnc "swhsFull" (progName `with'` phsChgMtrl)
-- I want to include SI as an acronym, but I can't find a way for the 
-- description to have accents when using dcc.

---ConceptChunks---

charging, coil, discharging, gauss_div, heat_flux, mech_energy,
  perfect_insul, phase_change_material, specific_heat, swhs_pcm, tank,
  tank_pcm, transient, water :: ConceptChunk
swhsProg :: NPNC
--FIXME: There are too many "swhs" chunks for very minor differences.
  
charging = dcc "charging" (nounPhraseSP "charging") "Charging of the tank"
coil = dcc "coil" (cn' "heating coil")
  "Coil in tank that heats by absorbing solar energy"
discharging = dcc "discharging" (nounPhraseSP "discharging") 
  "Discharging of the tank"
gauss_div = dcc "gauss_div" (nounPhraseSP "gauss's divergence theorem") fixme
heat_flux = dcc "heat_flux" (nounPhraseSP "heat flux") 
  "The rate of heat energy transfer per unit area"
  --FIXME: Heat flux needs to be a Unital Chunk
mech_energy = dcc "mech_energy" (nounPhraseSP "mechanical energy")
  "The energy that comes from motion and position"
--TODO: Physical property.
perfect_insul = dcc "perfect_insul" (nounPhraseSP "perfectly insulated")
  "Describes the property of a material not allowing heat transfer through its boundaries"
--FIXME: Remove " (PCM)" from the term and add an acronym instead.                
phase_change_material = dcc "pcm" (nounPhraseSP "Phase Change Material (PCM)")
      ("A substance that uses phase changes (such as melting) to absorb or " ++
      "release large amounts of heat at a constant temperature")
      
--FIXME: Temporarily have to manually create the compound phrase, because it
--uses acronym and a sentence.
swhsProg = npnc' "swhsProg" (nounPhrase'' (short progName +:+ 
  (phrase $ program ^. term)) (short progName +:+ (phrase $ program ^. term))
  CapFirst CapWords) "SWHS"
specific_heat = dcc "specific_heat" (nounPhraseSP "specific heat")
  "Heat capacity per unit mass" 
  --FIXME: Specific Heat needs to be a UnitalChunk
swhs_pcm = dcc "swhs_pcm" (nounPhraseSP 
  "solar water heating systems incorporating PCM")
  "Solar water heating systems incorporating phase change material"
tank = dcc "tank" (cn' "tank") "Solar water heating tank"
tank_pcm = dcc "tank_pcm" 
  (nounPhraseSP "solar water heating tank incorporating PCM")
  "FIXME: Define this"
transient = dcc "transient" (nounPhraseSP "transient") "Changing with time"
water = dcc "water" (cn' "water") "The liquid with which the tank is filled"
