{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.Concepts where

import Language.Drasil

---Acronyms---
ordDiffEq,phsChgMtrl,rightSide,progName :: ConceptChunk

ordDiffEq   = dcc "ordDiffEq" "ODE" "Ordinary Differential Equation"
phsChgMtrl  = dcc "phsChgMtrl" "PCM" "Phase Change Material"
rightSide   = dcc "rightSide" "RHS" "Right Hand Side"
progName    = dcc "progName" "SWHS" "Solar Water Heating System"

-- I want to include SI as an acronym, but I can't find a way for the 
-- description to have accents when using makeCC.

---NamedChunks---

charging, coil, discharging, gauss_div, heat_flux, mech_energy,
  perfect_insul, phase_change_material, specific_heat, swhs_pcm, swhsProg, tank,
  tank_pcm, transient, water :: NamedChunk

charging = makeCC "Charging" "Charging of the tank"
coil = makeCC "Heating coil" "Coil in tank that heats by absorbing solar energy"
discharging = makeCC "Discharging" "Discharging of the tank"
gauss_div = makeCC "Gauss's Divergence theorem" "Gauss's Divergence theorem"
heat_flux = makeCC "Heat flux" "The rate of heat energy transfer per unit area."
  --FIXME: Heat flux needs to be a Unital Chunk
mech_energy = makeCC "Mechanical energy" ("The energy that comes from motion" ++
              " and position")
perfect_insul = makeCC "perfectly insulated" ("Describes the property of a " ++
                "material not allowing heat transfer through its boundaries")
phase_change_material = makeCC "Phase Change Material (PCM)" ("A substance " ++
                        "that uses phase changes (such as melting) to absorb or " ++
                        "release large amounts of heat at a constant " ++
                        "temperature.")
swhsProg = makeCC "SWHS" "SWHS program"
specific_heat = makeCC "Specific heat" "Heat capacity per unit mass." 
  --FIXME: Specific Heat needs to be a UnitalChunk
swhs_pcm = makeCC "solar water heating systems incorporating PCM" ("Solar " ++
           "water heating systems incorporating phase change material")
tank = makeCC "Tank" "Solar water heating tank"
tank_pcm = makeCC "Solar water heating tank incorporating PCM" ("solar water" ++
           " heating tank incorporating PCM")
transient = makeCC "Transient" "Changing with time."
water = makeCC "Water" "The liquid with which the tank is filled"