module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (source, theory)
import Data.Drasil.Concepts.Physics (energy)

import Control.Lens((^.))

thermocon :: [ConceptChunk]
thermocon = [boiling, boil_pt, degree_', lawConsEnergy, lawConvCooling, latentHeat, melting, meltPt, phaseChange,
  sensHeat, temp, thermalAnalysis, thermalConduction, thermal_energy,
  thermalConductor, heat, heat_cap_spec, htFlux, heat_trans, ht_trans_theo, ener_src]

boiling, boil_pt, degree_', lawConsEnergy, lawConvCooling, latentHeat, melting, meltPt, phaseChange,
  sensHeat, temp, thermalAnalysis, thermalConduction, thermal_energy,
  thermalConductor, heat, heat_cap_spec, htFlux, heat_trans, ht_trans_theo, ener_src :: ConceptChunk

  
-- FIXME: "Boiling" is not a noun. How should we deal with it?
--    Same for "Melting"
boiling             = dcc "boiling"         (cn "boiling")
                      "Phase change from liquid to vapour"
boil_pt             = dcc "boil_pt"         (cn' "boiling point temperature")
                      ("Temperature at which a substance changes from liquid to " ++
                      "vapour")
degree_'            = dcc "degree"          (cn' "degree")
                      "A measure of the warmth or coldness of an object or substance"
heat                = dcc "heat"            (cn "heat")
                      ("Noun: The amount of heat energy inside a body. " ++
                      "Verb: To transfer thermal energy to a body")
heat_trans          = dcc "heat_trans"      (cn' "heat transfer")
                      ("The generation, use, conversion, and exchange of thermal " ++
                      "energy and heat between physical systems")
heat_cap_spec       = dcc "heat_cap_spec"   (cnIES "specific heat capacity")
                      ("The amount of energy required to raise the temperature " ++
                      "of the unit mass of a given substance by a given amount")
htFlux             = dcc "htFlux"         (cn'' "heat flux") 
                      ("The rate of thermal energy transfer through a given " ++
                      "surface per unit time")
latentHeat         = dcc "latentHeat"     (cn' "latent heat")
                      ("The heat required to convert a solid into a liquid or " ++
                      "vapor, or a liquid into a vapor, without change of temperature")
lawConsEnergy     = dcc "lawConsEnergy" 
                      (nounPhraseSP "law of conservation of energy" )
                      "Energy is conserved"
lawConvCooling    = dcc "lawConvCooling" 
                      (nounPhraseSP "Newton's law of cooling")
                      "Newton's law of convective cooling"
melting             = dcc "melting"         (cn "melting")
                      "Phase change from solid to liquid"
meltPt             = dcc "meltPt"         (cn' "melting point temperature")
                      "Temperature at which a substance changes from liquid to vapour"
phaseChange        = dcc "phaseChange"    (cn' "phase change")
                      "Change of state"
sensHeat           = dcc "sensHeat"       (cn' "sensible heat")
                      ("Heat exchanged by a body in which the exchange of heat " ++
                      "changes the temperature and some macroscopic variables of " ++
                      "the body or system, but leaves others unchanged")
temp                = dcc "temperature"     (cn' "temperature")
                      "The degree or intensity of heat present in a substance or object"
thermalAnalysis    = dcc "thermalAnalysis" 
                      (cnIP "thermal analysis" (IrregPlur (\x -> init (init x) ++ "es")))
                      "The study of material properties as they change with temperature"
thermalConduction  = dcc "thermalConduction"
                      (nounPhraseSP "thermal conduction")
                      "The transfer of heat energy through a substance"
thermalConductor   = dcc "thermalConductor"
                      (cn' "thermal conductor")
                      "An object through which thermal energy can be transferred easily"
thermal_energy      = dcc "thermal_energy" 
                      (cnIES "thermal energy") "The energy that comes from heat"

ener_src            = dcc "enerSrc" (compoundPhrase' (energy ^. term)
                      (source ^. term))
                      "A source from which useful energy can be extracted"
ht_trans_theo       = dcc "ht_trans_theo" (compoundPhrase' (heat_trans ^. term)
                      (theory ^. term))
                      ("Theory predicting the energy transfer that may take " ++
                      "place between material bodies as a result of temperature difference")