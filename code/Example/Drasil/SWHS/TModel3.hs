module Drasil.SWHS.TModel3 where

import Data.Char (toLower)

import Drasil.SWHS.Unitals
import Drasil.SWHS.DataDefs

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics

import Control.Lens ((^.))

s4_2_2_T3 :: Contents
s4_2_2_T3 = Definition (Theory t3LatHtE)

t3LatHtE :: RelationChunk
t3LatHtE = makeRC "Latent heat energy" t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = FCall (C latentE) [C time] := UnaryOp (Integral (Just (Low 0),
            Just (High (C time))) (Deriv Total (FCall (C latentE) [C tau]) (C tau)) tau)

-- Integrals need dTau at end
-- Deriv is specifically partial derivative... how to do regular derivative?
-- How to have conditions on a single equation

t3descr :: Sentence
t3descr = (P (latentE ^. symbol) :+: S " is the change in " :+:
          (sMap (map toLower) (S (thermal_energy ^. name))) :+: S " (" :+:
          Sy (joule ^. unit) :+: S "), " :+: (sMap (map toLower) 
          (S (latent_heat ^. name))) :+: S " energy. <Integral> is the rate" :+:
          S " of change of " :+: P (latentE ^. symbol) :+: S " with respect" :+:
          S " to " :+: (time ^. descr) :+: S " " :+: P (tau ^. symbol) :+: 
          S " (" :+: Sy (tau ^. unit) :+: S "). " :+: P (time ^. symbol) :+:
          S " is the " :+: (time ^. descr) :+: S " (" :+: Sy (time ^. unit) :+:
          S ") elapsed, as long as the " :+: (sMap (map toLower)
          (phase_change ^. descr)) :+: S " is not complete. The status of " :+:
          S "the " :+: (sMap (map toLower) (phase_change ^. descr)) :+:
          S " depends on the " :+: (melt_frac ^. descr) :+: S ", " :+: 
          makeRef s4_2_4_DD3 :+: S ". " :+: P (temp_melt ^. symbol) :+:
          S " and " :+: P (temp_boil ^. symbol) :+: S " are the " :+:
          (temp_melt ^. descr) :+: S " and " :+: (temp_boil ^. descr) :+:
          S ", respectively (" :+: Sy (temp ^. unit) :+: S "). " :+:
          (latent_heat ^. descr) :+: S " stops when all material has " :+:
          S "changed to the new phase.")
          
-- Wrong DD reference above, change when DD4 is available