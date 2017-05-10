{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.TModel1 where

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts

import Language.Drasil
import Prelude hiding (id)
import Data.Drasil.Concepts.Thermodynamics hiding (temp)
import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.Thermodynamics (temp)
import Data.Drasil.Quantities.PhysicalProperties
import Data.Drasil.Quantities.Physics (time)

import Control.Lens ((^.))

s4_2_2_T1 :: Contents
s4_2_2_T1 = Definition (Theory t1ConsThermE)

t1ConsThermE :: RelationConcept
t1ConsThermE = makeRC "t1ConsThermE" (nounPhraseSP "Conservation of thermal energy") 
  t1descr consThermERel

consThermERel :: Relation
consThermERel = (Neg (C gradient)) :. (C thFluxVect) + (C vol_ht_gen) :=
  (C density) * (C htCap) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = (S "The above equation gives the" +:+ (sLower
  (phrase $ law_cons_energy ^. term)) +:+ S "for" +:+ (sLower
  ((phrase $ transient ^. term))) +:+ (sLower (phrase $ heat_trans ^. term)) +:+
  S "in a material of" +:+ (phrase $ htCap ^. term) +:+ 
  P (htCap ^. symbol) +:+ S "(" :+: Sy (unit_symb htCap) :+: S ")" +:+
  S "and" +:+ (phrase $ density ^. term) `sC`
  P (density ^. symbol) +:+ S "(" :+: Sy (unit_symb density) :+: 
  S "), where" +:+ P (thFluxVect ^. symbol) +:+ S "is the" +:+ 
  (phrase $ thFluxVect ^. term) +:+ S "(" :+: Sy (unit_symb thFluxVect) :+:
  S "), " :+: P (vol_ht_gen ^. symbol) +:+ S "is the" +:+ 
  (phrase $ vol_ht_gen ^. term) +:+ S "(" :+: Sy (unit_symb vol_ht_gen) :+: 
  S "), " :+: P (temp ^. symbol) +:+ S "is the" +:+ 
  (phrase $ temp ^. term) +:+ S "(" :+: Sy (unit_symb temp) :+: S "), " :+: 
  P (time ^. symbol) +:+ S "is" +:+ (phrase $ time ^. term) +:+ S "(" :+: 
  Sy (unit_symb time) :+: S "), and" +:+ P (gradient ^. symbol) +:+ 
  S "is the" +:+. (gradient ^. defn) +:+ S "For this equation" +:+ 
  S "to apply, " :+: S "other forms of energy, such as" +:+
  (sLower(phrase $ mech_energy ^. term)) :+: 
  S ", are assumed to be negligible in the system (A1).")

--referencing within a simple list is not yet implemented.
--Forgot many "S" and ":+:" typing out above description
---- lost a lot of time fixing
