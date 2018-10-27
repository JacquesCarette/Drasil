module Drasil.SWHS.TMods (swhsTMods, consThermE, sensHtE, latentHtE) where

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (system)
import Data.Drasil.Concepts.Math (equation, rOfChng)
import Data.Drasil.Concepts.Physics (mech_energy)
import Data.Drasil.Concepts.Thermodynamics (phase_change, thermal_energy,
  heat_trans, law_cons_energy)

import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.PhysicalProperties (mass, density)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (temp, heat_cap_spec,
  latent_heat, melt_pt, boil_pt, sens_heat, heat_cap_spec)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma),
    foldlList, foldlSent, isThe, sAnd)
import Data.Drasil.SI_Units (joule)

import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (dd3HtFusion)
import Drasil.SWHS.Labels (thermalEnergyOnlyL)
import Drasil.SWHS.Unitals (melt_frac, tau, deltaT, htCap_V, htCap_S,
  htCap_L, vol_ht_gen, thFluxVect)

swhsTMods :: [TheoryModel]
swhsTMods = [consThermE, sensHtE, latentHtE]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm' consThermE_rc
  (tc' [qw thFluxVect, qw gradient, qw vol_ht_gen, 
    qw density, qw heat_cap_spec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [consThermERel] [] [makeRef consThemESrc]) 
  (mkLabelSame "consThermE" (Def TM)) [consThermEdesc]

consThermE_rc :: RelationConcept
consThermE_rc = makeRC "consThermE_rc"
  (nounPhraseSP "Conservation of thermal energy") consThermEdesc consThermERel 
  (mkLabelSame "ConsThermE" (Def TM))

consThermERel :: Relation
consThermERel = (negate (sy gradient)) $. (sy thFluxVect) + (sy vol_ht_gen) $=
  (sy density) * (sy heat_cap_spec) * (pderiv (sy temp) time)

consThemESrc :: Label
consThemESrc = mkURILabel "consThemESrc" "http://www.efunda.com/formulae/heat_transfer/conduction/overview_cond.cfm" "Fourier Law of Heat Conduction and Heat Equation"

consThermEdesc :: Sentence
consThermEdesc = foldlSent [
  S "The above", phrase equation, S "gives the", phrase law_cons_energy, S "for",
  phrase transient, phrase heat_trans, S "in a material of", phrase heat_cap_spec,
  ch heat_cap_spec, sParen (Sy (unit_symb heat_cap_spec)) `sAnd` phrase density `sC`
  ch density, sParen (Sy (unit_symb density)) `sC` S "where" +:+. 
  foldlList Comma List [ch thFluxVect `isThe` phrase thFluxVect +:+ sParen (Sy (unit_symb thFluxVect)),
  ch vol_ht_gen `isThe` phrase vol_ht_gen +:+ sParen (Sy (unit_symb vol_ht_gen)),
  ch temp `isThe` phrase temp +:+ sParen (Sy (unit_symb temp)),
  ch time +:+ S "is" +:+ phrase time +:+ sParen (Sy (unit_symb time)), ch gradient +:+
  S "is the" +:+ (gradient ^. defn)], S "For this", phrase equation, S "to apply" `sC`
  S "other forms of", phrase energy `sC` S "such as", phrase mech_energy `sC`
  S "are assumed to be negligible in the", phrase system, sParen (makeRefS thermalEnergyOnlyL)]

-------------------------
-- Theoretical Model 2 --
-------------------------
sensHtE :: TheoryModel
sensHtE = tm' sensHtE_rc
  (tc' [qw sens_heat, qw htCap_S, qw mass, 
    qw deltaT, qw melt_pt, qw temp, qw htCap_L, qw boil_pt, qw htCap_V] ([] :: [ConceptChunk])
  [] [sensHtEEqn] [] [makeRef sensHtESrc]) 
  (mkLabelSame "sensHtE" (Def TM)) [sensHtEdesc]

sensHtE_rc :: RelationConcept
sensHtE_rc = makeRC "sensHtE_rc" (nounPhraseSP "Sensible heat energy") sensHtEdesc sensHtEEqn
  (mkLabelSame "SensHtE" (Def TM))

sensHtESrc :: Label
sensHtESrc = mkURILabel "consThemESrc" "http://en.wikipedia.org/wiki/Sensible_heat" "Definition of Sensible Heat"

sensHtEEqn :: Relation
sensHtEEqn = (sy sens_heat) $= case_ [((sy htCap_S) * (sy mass) * (sy deltaT),
  ((sy temp) $< (sy melt_pt))), ((sy htCap_L) *
  (sy mass) * (sy deltaT), ((sy melt_pt) $< (sy temp) $<
  (sy boil_pt))), ((sy htCap_V) * (sy mass) *
  (sy deltaT), ((sy boil_pt) $< (sy temp)))]

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [
  ch sens_heat `isThe` S "change in",
  phrase sens_heat, phrase energy +:+. sParen (Sy (joule ^. usymb)),
  ch htCap_S `sC` ch htCap_L `sC` ch htCap_V, S "are the",
  phrase htCap_S `sC` phrase htCap_L `sC` S "and", phrase htCap_V `sC`
  S "respectively" +:+. sParen (Sy (unit_symb heat_cap_spec)),
  ch mass `isThe` phrase mass +:+. sParen (Sy (unit_symb mass)),
  ch temp `isThe` phrase temp,
  sParen (Sy (unit_symb temp)) `sC` S "and", ch deltaT `isThe`
  phrase deltaT +:+. sParen (Sy (unit_symb deltaT)),
  ch melt_pt `sAnd` ch boil_pt,
  S "are the", phrase melt_pt `sAnd` phrase boil_pt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start sens_heat :+: S "ing occurs as long as the material does",
  S "not reach a", phrase temp, S "where a" +:+
  phrase phase_change, S "occurs. A",
  phrase phase_change, S "occurs if",
  ch temp :+: S "=" :+: ch boil_pt,
  S "or", ch temp :+: S "=" +:+. ch melt_pt,
  S "If this" `isThe` S "case, refer to",
  (makeRefS latentHtE) `sC` at_start latent_heat,
  phrase energy]
 
--How to have new lines in the description?
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?

-------------------------
-- Theoretical Model 3 --
-------------------------
latentHtE :: TheoryModel
latentHtE = tm' latentHtE_rc
  (tc' [qw latent_heat, qw time, qw tau] ([] :: [ConceptChunk])
  [] [latHtEEqn] [] [makeRef latHtESrc]) (mkLabelSame "latentHtE" (Def TM)) [latentHtEdesc]

latentHtE_rc :: RelationConcept
latentHtE_rc = makeRC "latentHtE_rc"
  (nounPhraseSP "Latent heat energy") latentHtEdesc latHtEEqn 
  (mkLabelSame "LatHtE" (Def TM))

latHtEEqn :: Relation
latHtEEqn = apply1 latent_heat time $= 
  defint (eqSymb tau) 0 (sy time) (deriv (apply1 latent_heat tau) tau)

-- Integrals need dTau at end

latHtESrc :: Label
latHtESrc = mkURILabel "consThemESrc" "http://en.wikipedia.org/wiki/Latent_heat" "Definition of Latent Heat"

latentHtEdesc :: Sentence
latentHtEdesc = foldlSent [
  ch latent_heat `isThe` S "change in",
  phrase thermal_energy, sParen (Sy (joule ^. usymb)) `sC`
  phrase latent_heat +:+. phrase energy, 
  E latHtEEqn `isThe` phrase rOfChng, S "of",
  ch latent_heat, S "with respect to", phrase time,
  ch tau +:+. sParen (Sy (unit_symb tau)), ch time `isThe`
  phrase time, sParen (Sy (unit_symb time)),
  S "elapsed, as long as the",
  phrase phase_change, S "is not complete. The status of",
  S "the", phrase phase_change,
  S "depends on the", phrase melt_frac `sC`
  (makeRefS dd3HtFusion) :+: S ".",
  ch melt_pt `sAnd` ch boil_pt, S "are the",
  phrase melt_pt `sAnd` phrase boil_pt `sC`
  S "respectively" +:+. sParen (Sy (unit_symb temp)),
  at_start latent_heat :+: S "ing stops when all material has",
  S "changed to the new phase"]
