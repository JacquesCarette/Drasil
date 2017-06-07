{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Drasil.SWHS.TModel1 where

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts

import Language.Drasil
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation (assumption, system)
import Data.Drasil.Concepts.Thermodynamics hiding (temp, heat_cap_spec)
import Data.Drasil.Concepts.Physics (mech_energy)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.Thermodynamics (temp, heat_cap_spec)
import Data.Drasil.Quantities.PhysicalProperties
import Data.Drasil.Quantities.Physics (energy, time)
import Drasil.SWHS.DataDefs

import Control.Lens ((^.))

s4_2_2_T1 :: [Contents]
s4_2_2_T1 = map swhsSymbMapT [t1ConsThermE]

t1ConsThermE :: RelationConcept
t1ConsThermE = makeRC "t1ConsThermE" (nounPhraseSP "Conservation of thermal energy") 
  t1descr consThermERel

consThermERel :: Relation
consThermERel = (Neg (C gradient)) :. (C thFluxVect) + (C vol_ht_gen) :=
  (C density) * (C heat_cap_spec) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = (S "The above" +:+ phrase equation +:+ S "gives the" +:+
  phrase law_cons_energy +:+ S "for" +:+
  phrase transient +:+ phrase heat_trans +:+
  S "in a material of" +:+ phrase heat_cap_spec +:+ 
  P (heat_cap_spec ^. symbol) +:+ sParen (Sy (unit_symb heat_cap_spec)) +:+
  S "and" +:+ phrase density `sC`
  P (density ^. symbol) +:+ sParen (Sy (unit_symb density)) `sC` 
  S "where" +:+ P (thFluxVect ^. symbol) +:+ S "is the" +:+ 
  phrase thFluxVect +:+ sParen (Sy (unit_symb thFluxVect)) `sC`
  P (vol_ht_gen ^. symbol) +:+ S "is the" +:+ 
  phrase vol_ht_gen +:+ sParen (Sy (unit_symb vol_ht_gen)) `sC` 
  P (temp ^. symbol) +:+ S "is the" +:+ 
  phrase temp +:+ sParen (Sy (unit_symb temp)) `sC` 
  P (time ^. symbol) +:+ S "is" +:+ phrase time +:+
  sParen (Sy (unit_symb time)) `sC` S "and" +:+ P (gradient ^. symbol) +:+ 
  S "is the" +:+. (gradient ^. defn) +:+ S "For this" +:+ phrase equation +:+
  S "to apply" `sC` S "other forms of" +:+ phrase energy `sC` S "such as" +:+
  phrase mech_energy `sC` 
  S "are assumed to be negligible in the" +:+ phrase system +:+.
  sParen (short assumption :+: S (show 1)))

--referencing within a simple list is not yet implemented.
--Forgot many "S" and ":+:" typing out above description
---- lost a lot of time fixing
