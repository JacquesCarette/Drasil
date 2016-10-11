{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.Concepts where

import Language.Drasil

---Acronyms---
ordDiffEq,phsChgMtrl,rightSide,progName :: ConceptChunk

ordDiffEq   = makeCC "ODE" "Ordinary Differential Equation"
phsChgMtrl  = makeCC "PCM" "Phase Change Material"
rightSide   = makeCC "RHS" "Right Hand Side"
progName    = makeCC "SWHS" "Solar Water Heating System"

-- I want to include SI as an acronym, but I can't find a way for the 
-- description to have accents when using makeCC.

---ConceptChunks---

charging, coil, discharging, gauss_div, matlab, mech_energy, os, perfect_insul,
  phase_change_material, program, swhs_pcm, tank, tank_pcm, transient, 
  water :: ConceptChunk

charging = makeCC "Charging" "Charging of the tank"
coil = makeCC "Heating coil" "Coil in tank that heats by absorbing solar energy"
discharging = makeCC "Discharging" "Discharging of the tank"
gauss_div = makeCC "Gauss's Divergence theorem" "Gauss's Divergence theorem"
matlab = makeCC "MATLAB" "MATLAB programming language"
mech_energy = makeCC "Mechanical energy" ("The energy that comes from motion" ++
              " and position")
os = makeCC "OS" "operating system"
perfect_insul = makeCC "perfectly insulated" ("Describes the property of a " ++
                "material not allowing heat transfer through its boundaries")
phase_change_material = makeCC "Phase Change Material (PCM)" ("A substance " ++
                        "that uses phase changes (such as melting) to absorb or " ++
                        "release large amounts of heat at a constant " ++
                        "temperature.")
program = makeCC "SWHS" "SWHS program"
swhs_pcm = makeCC "solar water heating systems incorporating PCM" ("Solar " ++
           "water heating systems incorporating phase change material")
tank = makeCC "Tank" "Solar water heating tank"
tank_pcm = makeCC "Solar water heating tank incorporating PCM" ("solar water" ++
           " heating tank incorporating PCM")
transient = makeCC "Transient" "Changing with time."
water = makeCC "Water" "The liquid with which the tank is filled"