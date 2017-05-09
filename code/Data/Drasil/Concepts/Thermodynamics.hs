module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil

boiling,law_cons_energy, law_conv_cooling, latent_heat, melting, phase_change,
  sens_heat, temperature, thermal_analysis, thermal_conduction, thermal_energy,
  thermal_conductor, heat, heat_cap_spec, heat_trans :: ConceptChunk

--To save space.
fixme :: String
fixme = "FIXME: MISSING DEFINITION"
  
-- FIXME: "Boiling" is not a noun. How should we deal with it?
--    Same for "Melting"
boiling             = dcc "boiling"         (cn "boiling")
                      "Phase change from liquid to vapour"
heat                = dcc "heat"            (cn "heat") fixme
heat_trans          = dcc "heat_trans"      (cn' "heat transfer") fixme
heat_cap_spec       = dcc "heat_cap_spec"   (cnIES "specific heat capacity") fixme
latent_heat         = dcc "latent_heat"     (cn' "latent heat") fixme
law_cons_energy     = dcc "law_cons_energy" 
                      (nounPhraseSP "law of conservation of energy" )
                      "Energy is conserved"
law_conv_cooling    = dcc "law_conv_cooling" 
                      (nounPhraseSP "Newton's law of cooling")
                      "Newton's law of convective cooling"
melting             = dcc "melting"         (cn "melting")
                      "Phase change from solid to liquid"
phase_change        = dcc "phase_change"    (cn' "phase change") "Change of state"
--FIXME: sens_heat's definition is useless.
sens_heat           = dcc "sens_heat"       (cn' "sensible heat") fixme
temperature         = dcc "temperature"     (cn' "temperature") fixme
thermal_analysis    = dcc "thermal_analysis" 
                      (cnIP "thermal analysis" (IrregPlur (\x -> init (init x) ++ "es")))
                      "The study of material properties as they change with temperature"
thermal_conduction  = dcc "thermal_conduction"
                      (nounPhraseSP "thermal conduction")
                      "The transfer of heat energy through a substance"
thermal_conductor   = dcc "thermal_conductor"
                      (cn' "thermal conductor")
                      ("An object through which thermal energy can be transferred")
thermal_energy      = dcc "thermal_energy" 
                      (cnIES "thermal energy") "The energy that comes from heat"
