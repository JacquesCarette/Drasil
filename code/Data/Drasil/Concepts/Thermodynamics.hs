module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil

boiling, heat_flux, heat_trans, latent_heat, law_conv_cooling, melting, 
  phase_change, sens_heat, specific_heat, thermal_analysis, thermal_conduction,
  thermal_conductor, thermal_energy :: ConceptChunk

boiling = makeCC "Boiling" 
  "Phase change from liquid to vapour"
heat_flux = makeCC "Heat flux" 
  "The rate of heat energy transfer per unit area."
heat_trans = makeCC "Heat transfer" 
  "heat transfer"
latent_heat = makeCC "Latent heat" 
  "Latent heating"
law_cons_energy = makeCC "Law of conservation of energy" 
  "Energy is conserved"
law_conv_cooling = makeCC "Newton's law of cooling" 
  "Newton's law of convective cooling"
melting = makeCC "Melting" 
  "Phase change from solid to liquid"
phase_change = makeCC "Phase change" "Change of state"
sens_heat = makeCC "Sensible heat" "Sensible heating"
specific_heat = makeCC "Specific heat" "Heat capacity per unit mass." 
  --Specific Heat needs to be removed and replaced with a UnitalChunk
thermal_analysis = makeCC "Thermal analysis" ("The study of material " ++
                   "properties as they change with temperature")
thermal_conduction = makeCC "Thermal conduction" ("The transfer of heat " ++
                     "energy through a substance.")
thermal_conductor = makeCC "Thermal conductor" ("An object through which " ++
                    "thermal energy can be transferred")
thermal_energy = makeCC "Thermal energy" "The energy that comes from heat."