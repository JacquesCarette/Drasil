{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.TModel1 where

import Example.Drasil.SWHS.Units
import Example.Drasil.SWHS.Unitals

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

s4_2_2_T1 :: LayoutObj
s4_2_2_T1 = Definition (Theory t1ConsThermE)

t1ConsThermE :: RelationChunk
t1ConsThermE = makeRC "Conservation of thermal energy" t1descr consThermERel

consThermERel :: Relation
consThermERel = (Neg (C gradient)) :. (C thFluxVect) + (C vol_ht_gen) :=
                (C density) * (C htCap) * (Deriv (C temp) (C time))

t1descr :: Sentence
t1descr = (S "The above equation gives the conservation of energy for transient" :+:
          S " heat transfer in a material of " :+: (htCap ^. descr) :+: S " " :+:
          U (htCap ^. symbol) :+: S " (" :+: Sy (htCap ^. unit) :+: S ") and " :+: 
          (density ^. descr) :+: S " " :+: U (density ^. symbol) :+: S " (" :+:
          Sy (density ^. unit) :+: S "), where " :+: U (thFluxVect ^. symbol) :+:
          S " is the " :+: (thFluxVect ^. descr) :+: S " (" :+: Sy (thFluxVect ^. unit) :+:
          S "), " :+: U (vol_ht_gen ^. symbol) :+: S " is the " :+: (vol_ht_gen ^. descr) :+:
          S " (" :+: Sy (vol_ht_gen ^. unit) :+: S "), " :+: U (temp ^. symbol) :+: 
          S " is the " :+: (temp ^. descr) :+: S " (" :+: Sy (temp ^. unit) :+: 
          S "), " :+: U (time ^. symbol) :+: S " is " :+: (time ^. descr) :+: 
          S " (" :+: Sy (time ^. unit) :+: S "), and " :+: U (gradient ^. symbol) :+:
          S " is the " :+: (gradient ^. descr) :+: S ". For this equation to apply, " :+:
          S "other forms of energy, such as mechanical energy, are assmed to be negligible" :+:
          S " in the system. (A1)")

--Should I try to reference everything? Would need to adjust descr of density for it to make sense.
--Looks like referencing within a simple list is not yet implemented.
--Forgot many "S" and ":+:" typing out above description, lost a lot of time fixing
