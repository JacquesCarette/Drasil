module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (source, theory)
import Data.Drasil.Concepts.Physics (energy)

import Control.Lens((^.))

thermocon :: [ConceptChunk]
thermocon = [boilPt, boiling, degree_', enerSrc, heat, heatCapSpec, heatTrans,
  htFlux, htTransTheo, latentHeat, lawConsEnergy, lawConvCooling, meltPt,
  melting, phaseChange, sensHeat, temp, thermalAnalysis, thermalConduction,
  thermalConductor, thermalEnergy]

boilPt, boiling, degree_', enerSrc, heat, heatCapSpec, heatTrans, htFlux,
  htTransTheo, latentHeat, lawConsEnergy, lawConvCooling, meltPt, melting,
  phaseChange, sensHeat, temp, thermalAnalysis, thermalConduction,
  thermalConductor, thermalEnergy :: ConceptChunk
  
-- FIXME: "Boiling" is not a noun. How should we deal with it?
--    Same for "Melting"
boiling           = dcc "boiling"           (cn "boiling")
                      "the phase change from liquid to vapour"
boilPt            = dcc "boilPt"           (cn' "boiling point temperature")
                      "the temperature at which a substance changes from liquid to vapour"
degree_'          = dcc "degree"            (cn' "degree")
                      "a measure of the warmth or coldness of an object or substance"
heat              = dcc "heat"              (cn "heat")
                      ("Noun: The amount of heat energy inside a body. " ++
                      "Verb: To transfer thermal energy to a body") -- FIXME: there shouldn't be two definitions in one
heatTrans         = dcc "heatTrans"         (cn' "heat transfer")
                      ("the generation, use, conversion, and exchange of thermal " ++
                      "energy and heat between physical systems")
heatCapSpec       = dcc "heatCapSpec"       (cnIES "specific heat capacity")
                      ("the amount of energy required to raise the temperature " ++
                      "of the unit mass of a given substance by a given amount")
htFlux            = dcc "htFlux"            (cn'' "heat flux") 
                      ("the rate of thermal energy transfer through a given " ++
                      "surface per unit time")
latentHeat        = dcc "latentHeat"        (cn' "latent heat")
                      ("the heat required to convert a solid into a liquid or " ++
                      "vapor, or a liquid into a vapor, without change of temperature")
lawConsEnergy     = dcc "lawConsEnergy"     (nounPhraseSP "law of conservation of energy")
                      "the law that energy is conserved"
lawConvCooling    = dcc "lawConvCooling"    (nounPhraseSP "Newton's law of cooling")
                      "Newton's law of convective cooling"
melting           = dcc "melting"           (cn "melting")
                      "the phase change from solid to liquid"
meltPt            = dcc "meltPt"            (cn' "melting point temperature")
                      "the temperature at which a substance changes from liquid to vapour"
phaseChange       = dcc "phaseChange"       (cn' "phase change")
                      "a change of state"
sensHeat          = dcc "sensHeat"          (cn' "sensible heat")
                      ("heat exchanged by a body in which the exchange of heat " ++
                      "changes the temperature and some macroscopic variables of " ++
                      "the body or system, but leaves others unchanged")
temp              = dcc "temperature"       (cn' "temperature")
                      "the degree or intensity of heat present in a substance or object"
thermalAnalysis   = dcc "thermalAnalysis"
                      (cnIP "thermal analysis" (IrregPlur (\x -> init (init x) ++ "es")))
                      "the study of material properties as they change with temperature"
thermalConduction = dcc "thermalConduction" (nounPhraseSP "thermal conduction")
                      "the transfer of heat energy through a substance"
thermalConductor  = dcc "thermalConductor"  (cn' "thermal conductor")
                      "an object through which thermal energy can be transferred easily"
thermalEnergy     = dcc "thermalEnergy"     (cnIES "thermal energy")
                      "the energy that comes from heat"

enerSrc           = dcc "enerSrc"     (compoundPhrase' (energy ^. term)    (source ^. term))
                      "a source from which useful energy can be extracted"
htTransTheo       = dcc "htTransTheo" (compoundPhrase' (heatTrans ^. term) (theory ^. term))
                      ("the theory predicting the energy transfer that may take " ++
                      "place between material bodies as a result of temperature difference")
