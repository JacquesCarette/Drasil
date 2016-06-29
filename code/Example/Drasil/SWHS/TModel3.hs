module Example.Drasil.SWHS.TModel3 where

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

s4_2_2_T3 :: Contents
s4_2_2_T3 = Definition (Theory t3LatHtE)

t3LatHtE :: RelationChunk
t3LatHtE = makeRC "Latent heat energy" t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = FCall (C latentE) [C time] := UnaryOp (Integral (Just 0, Just (C time))) (Deriv (FCall (C latentE) [C tau]) (C tau))

-- Integrals need dTau at end
-- Deriv is specifically partial derivative... how to do regular derivative?
-- How to have conditions on a single equation

t3descr :: Sentence
t3descr = (U (latentE ^. symbol) :+: S " is the change in thermal energy (" :+:
          Sy (joule ^. unit) :+: S "), latent heat energy. <Integral> is " :+:
          S "the rate of change of " :+: U (latentE ^. symbol) :+: S " with" :+:
          S " respect to time " :+: U (tau ^. symbol) :+: S " (" :+: Sy (tau ^. unit) :+:
          S "). " :+: U (time ^. symbol) :+: S " is the time (" :+: Sy (time ^. unit) :+:
          S ") elapsed, as long as the phase change is not complete. The " :+:
          S "status of the phase change depends on the melt fraction DD4. " :+:
          U (temp_melt ^. symbol) :+: S " and " :+: U (temp_boil ^. symbol) :+:
          S " are the melting and boiling points, respectively (" :+:
          Sy (temp ^. unit) :+: S "). Latent heating stops when all material" :+:
          S " has changed to the new phase.")