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

t3LatHtE :: RelationConcept
t3LatHtE = makeRC "t3LatHtE" (nounPhraseSP "Latent heat energy") t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = FCall (C latentE) [C time] := UnaryOp (Integral (Just (Low 0),
  Just (High (C time))) (Deriv Total (FCall (C latentE) [C tau]) (C tau)) tau)

-- Integrals need dTau at end
-- Deriv is specifically partial derivative... how to do regular derivative?
-- How to have conditions on a single equation

t3descr :: Sentence
t3descr = (P (latentE ^. symbol) :+: S " is the change in " :+:
  (sMap (map toLower) (phrase $ thermal_energy ^. term)) :+: S " (" :+:
  Sy (joule ^. usymb) :+: S "), " :+: (sMap (map toLower) 
  (phrase $ latent_heat ^. term)) :+: S " energy. " :+:
  S "FIXME: THE INTEGRAL FROM THE ABOVE EQUATION SHOULD GO HERE" :+: 
  S " is the rate" :+:
  S " of change of " :+: P (latentE ^. symbol) :+: S " with respect" :+:
  S " to " :+: (phrase $ time ^. term) :+: S " " :+: P (tau ^. symbol) :+: 
  S " (" :+: Sy (unit_symb tau) :+: S "). " :+: P (time ^. symbol) :+:
  S " is the " :+: (phrase $ time ^. term) :+: S " (" :+: Sy (unit_symb time) :+:
  S ") elapsed, as long as the " :+: (sMap (map toLower)
  (phrase $ phase_change ^. term)) :+: S " is not complete. The status of " :+:
  S "the " :+: (sMap (map toLower) (phrase $ phase_change ^. term)) :+:
  S " depends on the " :+: (phrase $ melt_frac ^. term) :+: S ", " :+: 
  makeRef s4_2_4_DD3 :+: S ". " :+: P (temp_melt ^. symbol) :+:
  S " and " :+: P (temp_boil ^. symbol) :+: S " are the " :+:
  (phrase $ temp_melt ^. term) :+: S " and " :+: (phrase $ temp_boil ^. term) :+:
  S ", respectively (" :+: Sy (unit_symb temp) :+: S "). " :+:
  (phrase $ latent_heat ^. term) :+: S "ing stops when all material has " :+:
  S "changed to the new phase.")
  
-- Wrong DD reference above, change when DD4 is available