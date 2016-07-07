module Example.Drasil.SWHS.TModel2 where

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals
import Example.Drasil.SWHS.TModel3

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

s4_2_2_T2 :: Contents
s4_2_2_T2 = Definition (Theory t2SensHtE)

t2SensHtE :: RelationChunk
t2SensHtE = makeRC "Sensible heat energy" t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (C sensHtE) := Case [((C htCap_S) * (C mass) * (C deltaT), ((C temp) :< (C temp_melt))),
                            ((C htCap_L) * (C mass) * (C deltaT), ((C temp_melt) :< (C temp) :< (C temp_boil))),
                            ((C htCap_V) * (C mass) * (C deltaT), ((C temp_boil) :< (C temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

t2descr :: Sentence
t2descr = (U (sensHtE ^. symbol) :+: S " is the change in sensible heat " :+:
          S "energy (" :+: Sy (joule ^. unit) :+: S "). " :+: U (htCap_S ^. symbol) :+: 
          S ", " :+: U (htCap_L ^. symbol) :+: S ", " :+: U (htCap_V ^. symbol) :+:
          S " are the specific heat capacities of a solid, liquid, and vapour, " :+:
          S "respectively (" :+: Sy (htCap ^. unit) :+: S "). " :+: U (mass ^. symbol) :+: 
          S " is the mass (" :+: Sy (mass ^. unit) :+: S "). " :+: U (temp ^. symbol) :+: 
          S " is the temperature (" :+: Sy (temp ^. unit) :+: S"), and " :+: 
          U (deltaT ^. symbol) :+: S " is the change in temperature (" :+: Sy (deltaT ^. unit) :+:
          S "). " :+: U (temp_melt ^. symbol) :+: S " and " :+: U (temp_boil ^. symbol) :+:
          S " are the melting and boiling points, respectively (" :+: Sy (temp ^. unit) :+:
          S "). Sensible heating occurs as long as the material does not " :+: 
          S "reach a temperature where a phase change occurs. A phase change" :+:
          S " occurs if " :+: U (temp ^. symbol) :+: S "=" :+: U (temp_boil ^. symbol) :+:
          S " or " :+: U (temp ^. symbol) :+: S "=" :+: U (temp_melt ^. symbol) :+: 
          S ". If this is the case, refer to " :+: makeRef s4_2_2_T3 :+: S ", Latent heat energy.")

--Can't reference sections from SWHSBody!!
----importing would cause a cyclic import
--How to have new lines in the description? 
--Can't have relation and equation chunks together since they are called in a list
----You can, you just can't map "Definition" over a list, you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?