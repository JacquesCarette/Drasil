module Example.Drasil.SWHS.TModel3 where

import Data.Char (toLower)

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals
import Example.Drasil.SWHS.DataDefs
import Example.Drasil.SWHS.Concepts

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
t3descr = (U (latentE ^. symbol) :+: S " is the change in " :+: (sMap (map toLower) (S (thermal_energy ^. name))) :+: 
          S " (" :+: Sy (joule ^. unit) :+: S "), " :+: (sMap (map toLower) (S (latent_heat ^. name))) :+: 
          S " energy. <Integral> is the rate of change of " :+: U (latentE ^. symbol) :+:
          S " with respect to " :+: (time ^. descr) :+: S " " :+: U (tau ^. symbol) :+: S " (" :+:
          Sy (tau ^. unit) :+: S "). " :+: U (time ^. symbol) :+: S " is the " :+:
          (time ^. descr) :+: S " (" :+: Sy (time ^. unit) :+:
          S ") elapsed, as long as the " :+: (sMap (map toLower) (S (phs_change ^. name))) :+: 
          S " is not complete. The status of the " :+: (sMap (map toLower) (S (phs_change ^. name))) :+:
          S " depends on the " :+: (melt_frac ^. descr) :+: S ", " :+: makeRef s4_2_4_DD4 :+:
          S ". " :+: U (temp_melt ^. symbol) :+: S " and " :+: U (temp_boil ^. symbol) :+:
          S " are the " :+: (temp_melt ^. descr) :+: S " and " :+: (temp_boil ^. descr) :+:
          S ", respectively (" :+: Sy (temp ^. unit) :+: S "). " :+: (latent_heat ^. descr) :+:
          S " stops when all material has changed to the new phase.")
          