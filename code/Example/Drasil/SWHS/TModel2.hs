module Drasil.SWHS.TModel2 where

import Data.Char (toLower)

import Drasil.SWHS.Unitals
import Drasil.SWHS.TModel3

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics
import qualified Data.Drasil.Quantities.Thermodynamics as QT
import Data.Drasil.Quantities.PhysicalProperties

import Control.Lens ((^.))

s4_2_2_T2 :: Contents
s4_2_2_T2 = Definition (Theory t2SensHtE)

t2SensHtE :: RelationConcept
t2SensHtE = makeRC "t2SensHtE" (nounPhraseSP "Sensible heat energy") 
  t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (C sensHtE) := Case [((C htCap_S) * (C mass) * (C deltaT), 
  ((C QT.temp) :< (C temp_melt))), ((C htCap_L) * 
  (C mass) * (C deltaT), ((C temp_melt) :< (C QT.temp) :<
  (C temp_boil))), ((C htCap_V) * (C mass) * 
  (C deltaT), ((C temp_boil) :< (C QT.temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals 
-- were implemented incorrectly.
t2descr :: Sentence
t2descr = (P (sensHtE ^. symbol) :+: S " is the change in " :+:
  (sMap (map toLower) (phrase $ sens_heat ^. term)) :+: S " energy (" :+:
  Sy (joule ^. usymb) :+: S "). " :+: P (htCap_S ^. symbol) :+: 
  S ", " :+: P (htCap_L ^. symbol) :+: S ", " :+: 
  P (htCap_V ^. symbol) :+: S " are the " :+: (phrase $ htCap_S ^. term) :+: 
  S ", " :+: (phrase $ htCap_L ^. term) :+: S ", and " :+: 
  (phrase $ htCap_V ^. term) :+: S ", respectively (" :+: Sy (unit_symb htCap) :+:
  S "). " :+: P (mass ^. symbol) :+: S " is the " :+:
  (phrase $ mass ^. term) :+: S " (" :+: Sy (unit_symb mass) :+: S "). " :+:
  P (QT.temp ^. symbol) :+: S " is the " :+: (phrase $ temp ^. term) :+: S " (" :+:
  Sy (unit_symb QT.temp) :+: S "), and " :+: P (deltaT ^. symbol) :+:
  S " is the " :+: (phrase $ deltaT ^. term) :+: S " (" :+:
  Sy (unit_symb deltaT) :+: S "). " :+: P (temp_melt ^. symbol) :+: 
  S " and " :+: P (temp_boil ^. symbol) :+: S " are the " :+: 
  (phrase $ temp_melt ^. term) :+: S " and " :+: (phrase $ temp_boil ^. term) :+:
  S ", respectively (" :+: Sy (unit_symb QT.temp) :+: S "). " :+: 
  (phrase $ sens_heat ^. term) :+: S "ing occurs as long as the material does " :+: 
  S "not reach a " :+: (phrase $ temp ^. term) :+: S " where a " :+: 
  (sMap (map toLower) (phrase $ phase_change ^. term)) :+: S " occurs. A " :+:
  (sMap (map toLower) (phrase $ phase_change ^. term)) :+: S " occurs if " :+:
  P (QT.temp ^. symbol) :+: S "=" :+: P (temp_boil ^. symbol) :+:
  S " or " :+: P (QT.temp ^. symbol) :+: S "=" :+: 
  P (temp_melt ^. symbol) :+: S ". If this is the case, refer to " :+: 
  makeRef s4_2_2_T3 :+: S ", " :+: (phrase $ latent_heat ^. term) :+: 
  S " energy.")
  

--How to have new lines in the description? 
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?
