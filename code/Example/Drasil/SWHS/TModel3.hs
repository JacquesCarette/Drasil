module Drasil.SWHS.TModel3 where

import Drasil.SWHS.Unitals
import Drasil.SWHS.DataDefs

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics
import qualified Data.Drasil.Quantities.Thermodynamics as QT
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Concepts.Math (rOfChng)

import Control.Lens ((^.))

s4_2_2_T3 :: [Contents]
s4_2_2_T3 = map swhsSymbMapT [t3LatHtE]

t3LatHtE :: RelationConcept
t3LatHtE = makeRC "t3LatHtE" (nounPhraseSP "Latent heat energy") t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = FCall (C QT.latent_heat) [C time] := UnaryOp (Integral (Just (Low 0),
  Just (High (C time))) (Deriv Total (FCall (C QT.latent_heat) [C tau]) (C tau)) tau)

-- Integrals need dTau at end
-- Deriv is specifically partial derivative... how to do regular derivative?
-- How to have conditions on a single equation

t3descr :: Sentence
t3descr = (P (QT.latent_heat ^. symbol) +:+ S "is the change in" +:+
  phrase thermal_energy +:+ S "(" :+:
  Sy (joule ^. usymb) :+: S ")," +:+
  phrase latent_heat +:+ S "energy." +:+
  S "FIXME: THE INTEGRAL FROM THE ABOVE EQUATION SHOULD GO HERE" +:+ 
  S "is the" +:+ phrase rOfChng +:+ S "of" +:+ P (QT.latent_heat ^. symbol) +:+ S "with respect" +:+
  S "to" +:+ phrase time :+: S "" +:+ P (tau ^. symbol) +:+ 
  S "(" :+: Sy (unit_symb tau) :+: S ")." +:+ P (time ^. symbol) +:+
  S "is the" +:+ phrase time +:+ S "(" :+: Sy (unit_symb time) :+:
  S ") elapsed, as long as the" +:+
  phrase phase_change +:+ S "is not complete. The status of" +:+
  S "the" +:+ phrase phase_change +:+
  S "depends on the" +:+ phrase melt_frac `sC`
  swhsSymbMapDRef dd3HtFusion :+: S "." +:+
  P (QT.melt_pt ^. symbol) +:+
  S "and" +:+ P (QT.boil_pt ^. symbol) +:+ S "are the" +:+
  phrase QT.melt_pt +:+ S "and" +:+ phrase QT.boil_pt `sC`
  S "respectively (" :+: Sy (unit_symb QT.temp) :+: S ")." +:+
  at_start latent_heat :+: S "ing stops when all material has" +:+
  S "changed to the new phase.")
  
-- Wrong DD reference above, change when DD4 is available