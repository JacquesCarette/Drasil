{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Concepts where

import Example.Drasil.SWHS.Units

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

---Acronyms---
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,
  phsChgMtrl,physSyst,requirement,rightSide,srs,progName,thModel]

assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ordDiffEq,phsChgMtrl,
  physSyst,requirement,rightSide,srs,progName,thModel :: ConceptChunk

assumption  = makeCC "A" "Assumption"
dataDefn    = makeCC "DD" "Data Definition"
genDefn     = makeCC "GD" "General Definition"
goalStmt    = makeCC "GS" "Goal Statement"
inModel     = makeCC "IM" "Instance Model"
likelyChg   = makeCC "LC" "Likely Change"
ordDiffEq   = makeCC "ODE" "Ordinary Differential Equation"
phsChgMtrl  = makeCC "PCM" "Phase Change Material"
physSyst    = makeCC "PS" "Physical System Description"
requirement = makeCC "R" "Requirement"
rightSide   = makeCC "RHS" "Right Hand Side"
srs         = makeCC "SRS" "Software Requirements Specification"
progName    = makeCC "SWHS" "Solar Water Heating System"
thModel     = makeCC "T" "Theoretical Model"

-- I want to include SI as an acronym, but I can't find a way for the 
-- description to have accents when using makeCC.

---ConceptChunks---

boiling, charging, coil, discharging, gaseous, gauss_div, heat_flux, heat_trans, 
  liquid, latent_heat, law_cons_energy, law_cooling, mech_energy, melting, 
  perfect_insul, phase_change_material, phs_change, sens_heat, solid, 
  specific_heat, swhs_pcm, tank, tank_pcm, thermal_analysis, thermal_conduction,
  thermal_conductor, thermal_energy, transient, water :: ConceptChunk

boiling = makeCC "Boiling" "Phase change from liquid to vapour"
charging = makeCC "Charging" "Charging of the tank"
coil = makeCC "Heating coil" "Coil in tank that heats by absorbing solar energy"
discharging = makeCC "Discharging" "Discharging of the tank"
gaseous = makeCC "Gas" "gaseous state"
gauss_div = makeCC "Gauss's Divergence theorem" "Gauss's Divergence theorem"
heat_flux = makeCC "Heat flux" "The rate of heat energy transfer per unit area."
heat_trans = makeCC "Heat transfer" "heat transfer"
latent_heat = makeCC "Latent heat" "Latent heating"
law_cons_energy = makeCC "Law of conservation of energy" "Energy is conserved"
law_cooling = makeCC "Newton's law of cooling" ("Newton's law of convective " ++
              "cooling")
liquid = makeCC "Liquid" "liquid state"
mech_energy = makeCC "Mechanical energy" ("The energy that comes from motion" ++
              " and position")
melting = makeCC "Melting" "Phase change from solid to liquid"
perfect_insul = makeCC "perfectly insulated" ("Describes the property of a " ++
                "material not allowing heat transfer through its boundaries")
phase_change_material = makeCC "Phase Change Material (PCM)" ("A substance " ++
                        "that uses phase changes (such as melting) to absorb or " ++
                        "release large amounts of heat at a constant " ++
                        "temperature.")
phs_change = makeCC "Phase change" "Change of state"
sens_heat = makeCC "Sensible heat" "Sensible heating"
solid = makeCC "Solid" "solid state"
specific_heat = makeCC "Specific heat" "Heat capacity per unit mass."
swhs_pcm = makeCC "solar water heating systems incorporating PCM" ("Solar " ++
           "water heating systems incorporating phase change material")
tank = makeCC "Tank" "Solar water heating tank"
tank_pcm = makeCC "Solar water heating tank incorporating PCM" ("solar water" ++
           " heating tank incorporating PCM")
thermal_analysis = makeCC "Thermal analysis" ("The study of material " ++
                   "properties as they change with temperature")
thermal_conduction = makeCC "Thermal conduction" ("The transfer of heat " ++
                     "energy through a substance.")
thermal_conductor = makeCC "Thermal conductor" ("An object through which " ++
                    "thermal energy can be transferred")
thermal_energy = makeCC "Thermal energy" "The energy that comes from heat."
transient = makeCC "Transient" "Changing with time."
water = makeCC "Water" "The liquid with which the tank is filled"