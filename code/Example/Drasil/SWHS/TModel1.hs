{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.TModel1 where

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts

import Language.Drasil
import Prelude hiding (id)
import Data.Drasil.Concepts.Thermodynamics
import Data.Drasil.Quantities.Math (gradient)

import Control.Lens ((^.))

s4_2_2_T1 :: Contents
s4_2_2_T1 = Definition (Theory t1ConsThermE)

t1ConsThermE :: RelationConcept
t1ConsThermE = makeRC "t1ConsThermE" "Conservation of thermal energy" 
  t1descr consThermERel

consThermERel :: Relation
consThermERel = (Neg (C gradient)) :. (C thFluxVect) + (C vol_ht_gen) :=
                (C density) * (C htCap) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = (S "The above equation gives the " :+: (sLower
          (law_cons_energy ^. term)) :+: S " for " :+: (sLower
          ((transient ^. term))) :+: S " " :+: (sLower (heat_trans ^. term)) :+:
          S " in a material of " :+: (htCap ^. term) :+: S " " :+: 
          P (htCap ^. symbol) :+: S " (" :+: Sy (htCap ^. usymb) :+: S ") " :+:
          S "and " :+: (density ^. term) :+: S ", " :+: 
          P (density ^. symbol) :+: S " (" :+: Sy (density ^. usymb) :+: 
          S "), where " :+: P (thFluxVect ^. symbol) :+: S " is the " :+: 
          (thFluxVect ^. term) :+: S " (" :+: Sy (thFluxVect ^. usymb) :+:
          S "), " :+: P (vol_ht_gen ^. symbol) :+: S " is the " :+: 
          (vol_ht_gen ^. term) :+: S " (" :+: Sy (vol_ht_gen ^. usymb) :+: 
          S "), " :+: P (temp ^. symbol) :+: S " is the " :+: 
          (temp ^. term) :+: S " (" :+: Sy (temp ^. usymb) :+: S "), " :+: 
          P (time ^. symbol) :+: S " is " :+: (time ^. term) :+: S " (" :+: 
          Sy (time ^. usymb) :+: S "), and " :+: P (gradient ^. symbol) :+: 
          S " is the " :+: (gradient ^. defn) :+: S ". For this equation " :+: 
          S "to apply, " :+: S "other forms of energy, such as " :+:
          (sLower(mech_energy ^. term)) :+: 
          S ", are assumed to be negligible in the system (A1).")

--referencing within a simple list is not yet implemented.
--Forgot many "S" and ":+:" typing out above description
---- lost a lot of time fixing
