{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Drasil.SWHS.TMods (tModels, t1ConsThermE, 
  theory_model_swhsTMods, theory_model_T1,
  t1ConsThermE_new, t2SensHtE_new, theory_model_T2, t2SensHtE) where

import Language.Drasil
import Control.Lens ((^.))

import Drasil.DocumentLanguage (mkAssump)
import Data.Drasil.Concepts.Documentation (system, acroNumGen)
import Data.Drasil.SI_Units (joule)

import Data.Drasil.Concepts.Thermodynamics (phase_change, thermal_energy,
  heat_trans, law_cons_energy)
import Data.Drasil.Concepts.Physics (mech_energy)
import Data.Drasil.Concepts.Math (equation, rOfChng)
import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.Thermodynamics (temp, heat_cap_spec,
  latent_heat, melt_pt, boil_pt, sens_heat, heat_cap_spec)
import Data.Drasil.Quantities.PhysicalProperties (mass, density)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Utils (getES)
import Data.Drasil.SentenceStructures (foldlSent, isThe)

import Drasil.SWHS.Unitals (melt_frac, tau, deltaT, htCap_V, htCap_S,
  htCap_L, vol_ht_gen, thFluxVect)
import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (dd3HtFusion)

tModels :: [RelationConcept]
tModels = [t1ConsThermE, t2SensHtE, t3LatHtE]

--s4_2_2_swhsTMods
theory_model_swhsTMods :: [Contents]
theory_model_swhsTMods = acroNumGen (theory_model_T1 ++ theory_model_T2 
  ++ theory_model_T3) 1

-------------------------
-- Theoretical Model 1 --
-------------------------
--s4_2_2_T1
------------- New Chunck -----------
t1ConsThermE_new :: TheoryModel
t1ConsThermE_new = tm (cw t1ConsThermE)
  (tc' "ConsThermE_new" [qw thFluxVect, qw gradient, qw vol_ht_gen, 
    qw density, qw heat_cap_spec, qw temp, qw time] ([] :: [FundUnit])
  [] [TCon Invariant consThermERel] [])

------------------------------------
theory_model_T1 :: [Contents]
theory_model_T1 = [reldefn t1ConsThermE]

t1ConsThermE :: RelationConcept
t1ConsThermE = makeRC "t1ConsThermE"
  (nounPhraseSP "Conservation of thermal energy") t1descr consThermERel

consThermERel :: Relation
consThermERel = (negate (sy gradient)) $. (sy thFluxVect) + (sy vol_ht_gen) $=
  (sy density) * (sy heat_cap_spec) * (pderiv (sy temp) time)

t1descr :: Sentence
t1descr = foldlSent [
  S "The above", phrase equation, S "gives the",
  phrase law_cons_energy, S "for",
  phrase transient, phrase heat_trans,
  S "in a material of", phrase heat_cap_spec,
  getES heat_cap_spec, sParen (Sy (unit_symb heat_cap_spec)),
  S "and", phrase density `sC`
  getES density, sParen (Sy (unit_symb density)) `sC`
  S "where", getES thFluxVect `isThe`
  phrase thFluxVect, sParen (Sy (unit_symb thFluxVect)) `sC`
  getES vol_ht_gen `isThe`
  phrase vol_ht_gen, sParen (Sy (unit_symb vol_ht_gen)) `sC`
  getES temp `isThe`
  phrase temp, sParen (Sy (unit_symb temp)) `sC`
  getES time, S "is", phrase time,
  sParen (Sy (unit_symb time)) `sC` S "and", getES gradient,
  S "is the" +:+. (gradient ^. defn), S "For this", phrase equation,
  S "to apply" `sC` S "other forms of", phrase energy `sC` S "such as",
  phrase mech_energy `sC`
  S "are assumed to be negligible in the", phrase system,
  sParen (makeRef (mkAssump "assump1" EmptyS))]

--referencing within a simple list is not yet implemented.

-------------------------
-- Theoretical Model 2 --
-------------------------
t2SensHtE_new :: TheoryModel
t2SensHtE_new = tm (cw t2SensHtE)
  (tc' "SensHtE_new" [qw sens_heat, qw htCap_S, qw mass, 
    qw deltaT, qw melt_pt, qw temp, qw htCap_L, qw boil_pt, qw htCap_V] ([] :: [FundUnit])
  [] [TCon Invariant sensHtEEqn] [])

--s4_2_2_T2
theory_model_T2 :: [Contents]
theory_model_T2 = [reldefn t2SensHtE]

t2SensHtE :: RelationConcept
t2SensHtE = makeRC "t2SensHtE"
  (nounPhraseSP "Sensible heat energy") t2descr sensHtEEqn

sensHtEEqn :: Relation
sensHtEEqn = (sy sens_heat) $= case_ [((sy htCap_S) * (sy mass) * (sy deltaT),
  ((sy temp) $< (sy melt_pt))), ((sy htCap_L) *
  (sy mass) * (sy deltaT), ((sy melt_pt) $< (sy temp) $<
  (sy boil_pt))), ((sy htCap_V) * (sy mass) *
  (sy deltaT), ((sy boil_pt) $< (sy temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
t2descr :: Sentence
t2descr = foldlSent [
  getES sens_heat `isThe` S "change in",
  phrase sens_heat, phrase energy +:+. sParen (Sy (joule ^. usymb)),
  getES htCap_S `sC` getES htCap_L `sC` getES htCap_V, S "are the",
  phrase htCap_S `sC` phrase htCap_L `sC` S "and", phrase htCap_V `sC`
  S "respectively" +:+. sParen (Sy (unit_symb heat_cap_spec)),
  getES mass `isThe` phrase mass +:+. sParen (Sy (unit_symb mass)),
  getES temp `isThe` phrase temp,
  sParen (Sy (unit_symb temp)) `sC` S "and", getES deltaT `isThe`
  phrase deltaT +:+. sParen (Sy (unit_symb deltaT)),
  getES melt_pt, S "and", getES boil_pt,
  S "are the", phrase melt_pt, S "and", phrase boil_pt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start sens_heat :+: S "ing occurs as long as the material does",
  S "not reach a", phrase temp, S "where a" +:+
  phrase phase_change, S "occurs. A",
  phrase phase_change, S "occurs if",
  getES temp :+: S "=" :+: getES boil_pt,
  S "or", getES temp :+: S "=" +. getES melt_pt,
  S "If this" `isThe` S "case, refer to",
  (makeRef $ reldefn t3LatHtE) `sC`
  at_start latent_heat, phrase energy]
 

--How to have new lines in the description?
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?

-------------------------
-- Theoretical Model 3 --
-------------------------
--s4_2_2_T3
theory_model_T3 :: [Contents]
theory_model_T3 = [reldefn t3LatHtE]

t3LatHtE :: RelationConcept
t3LatHtE = makeRC "t3LatHtE"
  (nounPhraseSP "Latent heat energy") t3descr latHtEEqn

latHtEEqn :: Relation
latHtEEqn = apply1 latent_heat time $= 
  defint (eqSymb tau) 0 (sy time) (deriv (apply1 latent_heat tau) tau)

-- Integrals need dTau at end

t3descr :: Sentence
t3descr = foldlSent [
  getES latent_heat `isThe` S "change in",
  phrase thermal_energy, sParen (Sy (joule ^. usymb)) `sC`
  phrase latent_heat +:+. phrase energy,
  E latHtEEqn `isThe` phrase rOfChng, S "of",
  getES latent_heat, S "with respect",
  S "to", phrase time, getES tau +:+.
  sParen (Sy (unit_symb tau)), getES time `isThe`
  phrase time, sParen (Sy (unit_symb time)),
  S "elapsed, as long as the",
  phrase phase_change, S "is not complete. The status of",
  S "the", phrase phase_change,
  S "depends on the", phrase melt_frac `sC`
  (makeRef $ datadefn dd3HtFusion) :+: S ".",
  getES melt_pt, S "and", getES boil_pt, S "are the",
  phrase melt_pt, S "and", phrase boil_pt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start latent_heat :+: S "ing stops when all material has",
  S "changed to the new phase"]
