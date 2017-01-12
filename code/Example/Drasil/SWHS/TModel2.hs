module Drasil.SWHS.TModel2 where

import Data.Char (toLower)

import Drasil.SWHS.Unitals
import Drasil.SWHS.TModel3

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics

import Control.Lens ((^.))

s4_2_2_T2 :: Contents
s4_2_2_T2 = Definition (Theory t2SensHtE)

t2SensHtE :: RelationChunk
t2SensHtE = makeRC "Sensible heat energy" t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (C sensHtE) := Case [((C htCap_S) * (C mass) * (C deltaT), 
                            ((C temp) :< (C temp_melt))), ((C htCap_L) * 
                            (C mass) * (C deltaT), ((C temp_melt) :< (C temp) :<
                            (C temp_boil))), ((C htCap_V) * (C mass) * 
                            (C deltaT), ((C temp_boil) :< (C temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.
--FIXME: Figure out why some of these are "term" and some are "defn".
--  The majority of them should be uniform.
t2descr :: Sentence
t2descr = (P (sensHtE ^. symbol) :+: S " is the change in " :+:
          (sMap (map toLower) (sens_heat ^. term)) :+: S " energy (" :+:
          Sy (joule ^. unit) :+: S "). " :+: P (htCap_S ^. symbol) :+: 
          S ", " :+: P (htCap_L ^. symbol) :+: S ", " :+: 
          P (htCap_V ^. symbol) :+: S " are the " :+: (htCap_S ^. defn) :+: 
          S ", " :+: (htCap_L ^. defn) :+: S ", and " :+: 
          (htCap_V ^. defn) :+: S ", respectively (" :+: Sy (htCap ^. unit) :+:
          S "). " :+: P (mass ^. symbol) :+: S " is the " :+:
          (mass ^. term) :+: S " (" :+: Sy (mass ^. unit) :+: S "). " :+:
          P (temp ^. symbol) :+: S " is the " :+: (temp ^. defn) :+: S " (" :+:
          Sy (temp ^. unit) :+: S "), and " :+: P (deltaT ^. symbol) :+:
          S " is the " :+: (deltaT ^. defn) :+: S " (" :+:
          Sy (deltaT ^. unit) :+: S "). " :+: P (temp_melt ^. symbol) :+: 
          S " and " :+: P (temp_boil ^. symbol) :+: S " are the " :+: 
          (temp_melt ^. defn) :+: S " and " :+: (temp_boil ^. defn) :+:
          S ", respectively (" :+: Sy (temp ^. unit) :+: S "). " :+: 
          (sens_heat ^. term) :+: S "ing occurs as long as the material does " :+: 
          S "not reach a " :+: (temp ^. defn) :+: S " where a " :+: 
          (sMap (map toLower) (phase_change ^. term)) :+: S " occurs. A " :+:
          (sMap (map toLower) (phase_change ^. term)) :+: S " occurs if " :+:
          P (temp ^. symbol) :+: S "=" :+: P (temp_boil ^. symbol) :+:
          S " or " :+: P (temp ^. symbol) :+: S "=" :+: 
          P (temp_melt ^. symbol) :+: S ". If this is the case, refer to " :+: 
          makeRef s4_2_2_T3 :+: S ", " :+: (latent_heat ^. term) :+: 
          S " energy.")
          

--How to have new lines in the description? 
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?
