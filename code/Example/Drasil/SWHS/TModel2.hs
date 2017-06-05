module Drasil.SWHS.TModel2 where

import Drasil.SWHS.Unitals
import Drasil.SWHS.TModel3

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics
import qualified Data.Drasil.Quantities.Thermodynamics as QT
import Data.Drasil.Quantities.PhysicalProperties
import Drasil.SWHS.DataDefs

import Control.Lens ((^.))

s4_2_2_T2 :: [Contents]
s4_2_2_T2 = map swhsSymbMapT [t2SensHtE]

t2SensHtE :: RelationConcept
t2SensHtE = makeRC "t2SensHtE" (nounPhraseSP "Sensible heat energy") 
  t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (C QT.sens_heat) := Case [((C htCap_S) * (C mass) * (C deltaT), 
  ((C QT.temp) :< (C QT.melt_pt))), ((C htCap_L) * 
  (C mass) * (C deltaT), ((C QT.melt_pt) :< (C QT.temp) :<
  (C QT.boil_pt))), ((C htCap_V) * (C mass) * 
  (C deltaT), ((C QT.boil_pt) :< (C QT.temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals 
-- were implemented incorrectly.
t2descr :: Sentence
t2descr = (P (QT.sens_heat ^. symbol) :+: S " is the change in " :+:
  (phrase $ sens_heat) :+: S " energy (" :+:
  Sy (joule ^. usymb) :+: S "). " :+: P (htCap_S ^. symbol) :+: 
  S ", " :+: P (htCap_L ^. symbol) :+: S ", " :+: 
  P (htCap_V ^. symbol) :+: S " are the " :+: (phrase $ htCap_S) :+: 
  S ", " :+: (phrase $ htCap_L) :+: S ", and " :+: 
  (phrase $ htCap_V) :+: S ", respectively (" :+: Sy (unit_symb QT.heat_cap_spec) :+:
  S "). " :+: P (mass ^. symbol) :+: S " is the " :+:
  (phrase $ mass) :+: S " (" :+: Sy (unit_symb mass) :+: S "). " :+:
  P (QT.temp ^. symbol) :+: S " is the " :+: (phrase $ temp) :+: S " (" :+:
  Sy (unit_symb QT.temp) :+: S "), and " :+: P (deltaT ^. symbol) :+:
  S " is the " :+: (phrase $ deltaT) :+: S " (" :+:
  Sy (unit_symb deltaT) :+: S "). " :+: P (QT.melt_pt ^. symbol) :+: 
  S " and " :+: P (QT.boil_pt ^. symbol) :+: S " are the " :+: 
  (phrase $ QT.melt_pt) :+: S " and " :+: (phrase $ QT.boil_pt) :+:
  S ", respectively (" :+: Sy (unit_symb QT.temp) :+: S "). " :+: 
  (at_start $ sens_heat) :+: S "ing occurs as long as the material does " :+: 
  S "not reach a " :+: (phrase $ temp) :+: S " where a " :+: 
  (phrase $ phase_change) :+: S " occurs. A " :+:
  (phrase $ phase_change) :+: S " occurs if " :+:
  P (QT.temp ^. symbol) :+: S "=" :+: P (QT.boil_pt ^. symbol) :+:
  S " or " :+: P (QT.temp ^. symbol) :+: S "=" :+: 
  P (QT.melt_pt ^. symbol) :+: S ". If this is the case, refer to " :+: 
  (makeRef (swhsSymbMapT t3LatHtE)) :+: S ", " :+: (at_start $ latent_heat) :+: 
  S " energy.")
  

--How to have new lines in the description? 
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?
