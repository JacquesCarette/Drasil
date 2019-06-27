module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (source, theory)
import Data.Drasil.Concepts.Physics (energy)

import Control.Lens((^.))

thermocon :: [ConceptChunk]
thermocon = [boiling, boilPt, degree_', lawConsEnergy, lawConvCooling,
  latentHeat, melting, meltPt, phaseChange, sensHeat, temp, thermalAnalysis,
  thermalConduction, thermalEnergy, thermalConductor, heat, heatCapSpec, htFlux,
  heatTrans, htTransTheo, enerSrc]

boiling, boilPt, degree_', lawConsEnergy, lawConvCooling, latentHeat, melting,
  meltPt, phaseChange, sensHeat, temp, thermalAnalysis, thermalConduction,
  thermalEnergy, thermalConductor, heat, heatCapSpec, htFlux, heatTrans,
  htTransTheo, enerSrc :: ConceptChunk


  
-- FIXME: "Boiling" is not a noun. How should we deal with it?
--    Same for "Melting"
boiling             = dcc "boiling"         (cn "boiling")
                      "Phase change from liquid to vapour"
boilPt             = dcc "boilPt"         (cn' "boiling point temperature")
                      ("Temperature at which a substance changes from liquid to " ++
                      "vapour")
degree_'            = dcc "degree"          (cn' "degree")
                      "A measure of the warmth or coldness of an object or substance"
heat                = dcc "heat"            (cn "heat")
                      ("Noun: The amount of heat energy inside a body. " ++
                      "Verb: To transfer thermal energy to a body")
heatTrans          = dcc "heatTrans"      (cn' "heat transfer")
                      ("The generation, use, conversion, and exchange of thermal " ++
                      "energy and heat between physical systems")
heatCapSpec       = dcc "heatCapSpec"   (cnIES "specific heat capacity")
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
thermalEnergy      = dcc "thermalEnergy" 
                      (cnIES "thermal energy") "The energy that comes from heat"

enerSrc            = dcc "enerSrc" (compoundPhrase' (energy ^. term)
                      (source ^. term))
                      "A source from which useful energy can be extracted"

htTransTheo       = dcc "htTransTheo" (compoundPhrase' (heatTrans ^. term)
                      (theory ^. term))
                      ("Theory predicting the energy transfer that may take " ++
                      "place between material bodies as a result of temperature difference")