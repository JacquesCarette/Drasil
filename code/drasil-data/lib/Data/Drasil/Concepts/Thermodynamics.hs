-- | Defines concepts used in the field of thermodynamics.
module Data.Drasil.Concepts.Thermodynamics where

import Language.Drasil
--import Utils.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.Database (mkUid)

import Data.Drasil.Concepts.Documentation (source, theory)
import Data.Drasil.Concepts.Physics (energy)

-- | Collects all thermodynamics-related concepts.
thermocon :: [ConceptChunk]
thermocon = [boiling, degree_', enerSrc, heat, heatTrans, htTransTheo,
  lawConsEnergy, lawConvCooling, melting, phaseChange, thermalAnalysis,
  thermalConduction, thermalConductor, thermalEnergy]

boilPt, boiling, degree_', enerSrc, heat, heatCapSpec, heatTrans, htFlux,
  htTransTheo, latentHeat, lawConsEnergy, lawConvCooling, meltPt, melting,
  phaseChange, sensHeat, temp, thermalAnalysis, thermalConduction,
  thermalConductor, thermalEnergy :: ConceptChunk

-- FIXME: "Boiling" is not a noun. How should we deal with it?
--    Same for "Melting"
boiling           = cncpt''' (mkUid "boiling")           (cn "boiling")
                      (S "the phase change from liquid to vapour")
boilPt            = cncpt''' (mkUid "boilPt")            (cn' "boiling point temperature")
                      (S "the temperature at which a substance changes from liquid to vapour")
degree_'          = cncpt''' (mkUid "thermo_degree")     (cn' "degree")
                      (S "a measure of the warmth or coldness of an object or substance")
heat              = cncpt''' (mkUid "heat")              (cn "heat")
                      (S ("Noun: The amount of heat energy inside a body. " ++
                      "Verb: To transfer thermal energy to a body")) -- FIXME: there shouldn't be two definitions in one
heatTrans         = cncpt''' (mkUid "heatTrans")         (cn' "heat transfer")
                      (S ("the generation, use, conversion, and exchange of thermal " ++
                      "energy and heat between physical systems"))
heatCapSpec       = cncpt''' (mkUid "heatCapSpec")       (cnIES "specific heat capacity")
                      (S ("the amount of energy required to raise the temperature " ++
                      "of the unit mass of a given substance by a given amount"))
htFlux            = cncpt''' (mkUid "htFlux")            (cn'' "heat flux")
                      (S ("the rate of thermal energy transfer through a given " ++
                      "surface per unit time"))
latentHeat        = cncpt''' (mkUid "latentHeat")        (cn' "latent heat")
                      (S ("the heat required to convert a solid into a liquid or " ++
                      "vapor, or a liquid into a vapor, without change of temperature"))
lawConsEnergy     = cncpt''' (mkUid "lawConsEnergy")     (nounPhraseSP "law of conservation of energy")
                      (S "the law that energy is conserved")
lawConvCooling    = cncpt''' (mkUid "lawConvCooling")    (nounPhraseSP "Newton's law of cooling")
                      (S "Newton's law of convective cooling")
melting           = cncpt''' (mkUid "melting")           (cn "melting")
                      (S "the phase change from solid to liquid")
meltPt            = cncpt''' (mkUid "meltPt")            (cn' "melting point temperature")
                      (S "the temperature at which a substance changes from liquid to vapour")
phaseChange       = cncpt''' (mkUid "phaseChange")       (cn' "phase change")
                      (S "a change of state")
sensHeat          = cncpt''' (mkUid "sensHeat")          (cn' "sensible heat")
                      (S ("heat exchanged by a body in which the exchange of heat " ++
                      "changes the temperature and some macroscopic variables of " ++
                      "the body or system, but leaves others unchanged"))
temp              = cncpt''' (mkUid "temperature")       (cn' "temperature")
                      (S "the degree or intensity of heat present in a substance or object")
thermalAnalysis   = cncpt''' (mkUid "thermalAnalysis")
                      (cnIP "thermal analysis" (IrregPlur (\x -> init (init x) ++ "es")))
                      (S "the study of material properties as they change with temperature")
thermalConduction = cncpt''' (mkUid "thermalConduction") (nounPhraseSP "thermal conduction")
                      (S "the transfer of heat energy through a substance")
thermalConductor  = cncpt''' (mkUid "thermalConductor")  (cn' "thermal conductor")
                      (S "an object through which thermal energy can be transferred easily")
thermalEnergy     = cncpt''' (mkUid "thermalEnergy")     (cnIES "thermal energy")
                      (S "the energy that comes from heat")

enerSrc           = cncpt''' (mkUid "enerSrc")     (combineNINI energy source)
                      (S "a source from which useful energy can be extracted")
htTransTheo       = cncpt''' (mkUid "htTransTheo") (combineNINI heatTrans theory)
                      (S ("the theory predicting the energy transfer that may take " ++
                      "place between material bodies as a result of temperature difference"))
