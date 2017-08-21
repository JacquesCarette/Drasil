module Drasil.NoPCM.TMods (t1consThermE) where

import Drasil.NoPCM.Unitals

import Language.Drasil
import Data.Drasil.Utils(getS)
import Data.Drasil.Quantities.PhysicalProperties
import Data.Drasil.Quantities.Thermodynamics (temp, heat_cap_spec)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.Math (gradient)

import Data.Drasil.SentenceStructures

----EqChunks----
--Theoretical models--
t1consThermE :: RelationConcept
t1consThermE = makeRC "t1consThermE" 
  (nounPhraseSP "Conservation of thermal energy")
  t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C heat_cap_spec) * (Deriv Part (C temp) (C time))
  
t1descr :: Sentence
t1descr = 
  foldlSent [S "This equation gives the conservation of energy for time",
  S "varying heat transfer in a material of specific heat capacity", 
  getS heat_cap_spec, S "and density", getS density `sC` S "where",
  getS thFluxVect, S "is the thermal flux vector,",
  getS ht_gen_vol, S "is the volumetric heat generation,",
  getS temp, S "is the temperature,", getS time,
  S "is time, and", getS gradient, S "is the gradient operator.",
  S "For this equation to apply, other forms of energy, such as mechanical",
  S "energy, are assumed to be negligible in the"] --FIXME: Add 'system (A1).'
                                                   --or Assumption 1 reference