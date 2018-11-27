module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, generalDefinitions,
  normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, normShrRGD, momentEqlGD,
  mobShearWOGD, resShearWOGD) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.DocLang.SRS as SRS (physSyst)

import Data.Drasil.Concepts.Documentation (assumption, definition, 
  method_, property, value, variable)
import Data.Drasil.Concepts.Math (normal, perp, surface)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)

import Data.Drasil.Quantities.Physics (force)
import Data.Drasil.Quantities.SolidMechanics (nrmStrss)

import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, getTandS, ofThe, sAnd)

import Drasil.SSP.Assumptions (newA2, newA3, newA4, newA5, newA6)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, momExpr)
import Drasil.SSP.DataDefs (lengthLs, sliceWght)
import Drasil.SSP.Defs (intrslce, slice, slope, slpSrf)
import Drasil.SSP.Labels (forceDiagramL)
import Drasil.SSP.Figures (fig_forceacting)
import Drasil.SSP.References (chen2005)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, fricAngle, fs, intNormForce, intShrForce, inxi, inxiM1, 
  mobShrI, normToShear, nrmFSubWat, scalFunc, shearFNoIntsl, shrResI, 
  shrResI, shrStress, totNrmForce, xi, shearRNoIntsl, shrResI, slcWght,
  surfHydroForce, surfLoad, surfAngle, impLoadAngle, earthqkLoadFctr,
  watrForceDif)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, resShearWOGD, 
  mobShearWOGD, normShrRGD, momentEqlGD]

normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, resShearWOGD, mobShearWOGD, normShrRGD, momentEqlGD :: GenDefn
normForcEqGD = gd'' normForcEq  [chen2005]   "normForcEq"  [nmFEq_desc]
bsShrFEqGD   = gd'' bsShrFEq    [chen2005]   "bsShrFEq"    [bShFEq_desc]
resShrGD     = gd'' resShr      [chen2005]   "resShr"      [resShr_desc]
mobShrGD     = gd'' mobShr      [chen2005]   "mobShr"      [mobShr_desc]
resShearWOGD = gd'' resShearWO  [chen2005]   "resShearWO"  []
mobShearWOGD = gd'' mobShearWO  [chen2005]   "mobShearWO"  []
normShrRGD   = gd'' normShrR    [chen2005]   "normShrR"    [nmShrR_desc]
momentEqlGD  = gd'' momentEql   [chen2005]   "momentEql"   [momEql_desc]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEq_desc nmFEq_rel -- genDef1Label

nmFEq_rel :: Relation
nmFEq_rel = inxi totNrmForce $= eqlExpr cos sin
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

nmFEq_desc :: Sentence
nmFEq_desc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the shear direction", at_start force, S "equilibrium is",
  S "derived from the free body diagram of", makeRef2S fig_forceacting,
  S "in", makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section])]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEq_desc bShFEq_rel -- genDef2Label

bShFEq_rel :: Relation
bShFEq_rel = inxi mobShrI $= eqlExprN sin cos
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

bShFEq_desc :: Sentence
bShFEq_desc = foldlSent [S "This equation satisfies", makeRef2S equilibrium +:+.
  S "in the shear direction", at_start force, S "equilibrium is",
  S "derived from the free body diagram of", makeRef2S fig_forceacting,
  S "in", makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section])]

--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat * tan (inxi fricAngle) + inxi cohesion *
  inxi baseWthX * sec (inxi baseAngle)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShr_desc resShr_rel -- genDef3Label

resShr_rel :: Relation
resShr_rel = inxi shrResI $= shrResEqn

resShr_desc :: Sentence
resShr_desc = foldlSent_ [S "The Mohr-Coulomb resistive shear strength of a",
  phrase slice, ch shrStress, S "from", makeRef2S mcShrStrgth,
  S "is multiplied by the area", E $ sy baseWthX * sec(sy baseAngle) * 1,
  S "to obtain the" +:+. getTandS shrResI, S "Note the extra", E 1,
  S "is to represent a unit of width which is multiplied by the",
  getTandS baseLngth, S "of the plane where the", phrase normal,
  S "occurs, where", (E $ sy baseLngth $= sy baseWthX * sec(sy baseAngle))
  `sAnd` ch baseWthX, S "is the x width of the base. This accounts for the",
  phrase nrmFSubWat, E $ sy nrmFSubWat $= sy totNrmForce - sy baseHydroForce,
  S "of a soil from", -- FIXME: add prime to nrmStrss
  makeRef2S effStress, S "where the", phrase nrmStrss,
  S "is multiplied by the same area to obtain the", phrase nrmFSubWat,
  E $ sy nrmStrss * sy baseWthX * sec(sy baseAngle) * 1 $= sy nrmFSubWat,
  makeRef2S newA3, makeRef2S newA4, makeRef2S newA5]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobile shear force") mobShr_desc mobShr_rel -- genDef4Label

mobShr_rel :: Relation
mobShr_rel = inxi mobShrI $= inxi shrResI / sy fs $= shrResEqn / sy fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent_ [
  S "From", phrase definition `ofThe` phrase fs, S "in", makeRef2S factOfSafety `sC`
  S "and the new", phrase definition, S "of", ch shrResI `sC` S "a new",
  S "relation for", S "net mobile" +:+ phrase shearForce `ofThe` phrase slice,
  ch shearFNoIntsl, S "is found as the resistive shear", ch shrResI,
  sParen (makeRef2S resShrGD), S "divided by the factor of safety" +:+. ch fs,
  makeRef2S newA2, makeRef2S newA3, makeRef2S newA4, makeRef2S newA5]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR"
  (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel -- genDef5Label

nmShrR_rel :: Relation
nmShrR_rel = sy intShrForce $= sy normToShear * sy scalFunc * sy intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent_ [S "The", phrase assumption,
  S "for the Morgenstern Price", phrase method_, sParen (makeRef2S newA6),
  S "that the", phrase intrslce, phrase shearForce, ch xi,
  S "is proportional to the", phrase intrslce, 
  phrase normForce, ch intNormForce, S "by a proportionality constant",
  ch normToShear, S "and a predetermined scaling function",
  ch scalFunc `sC` S "that changes",
  (S "proportionality as a function" `ofThe`
  S "x-ordinate position of the") +:+. phrase intrslce, ch scalFunc,
  S "is typically either a half-sine along the", phrase slpSrf `sC`
  S "or a constant"]

--
resShearWO :: RelationConcept
resShearWO = makeRC "resShearWO"
  (nounPhraseSP "resistive shear force") resShearWO_desc resShearWO_rel

resShearWO_rel :: Relation
resShearWO_rel = sy shearRNoIntsl $= (((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) + (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)

resShearWO_desc :: Sentence
resShearWO_desc = foldlSent_ [S "The", phrase assumption,
  S "for the Morgenstern Price", phrase method_, sParen (makeRef2S newA6),
  S "that the", phrase intrslce, phrase shearForce, ch xi,
  S "is proportional to the", phrase intrslce, 
  phrase normForce, ch intNormForce, S "by a proportionality constant",
  ch normToShear, S "and a predetermined scaling function",
  ch scalFunc `sC` S "that changes",
  (S "proportionality as a function" `ofThe`
  S "x-ordinate position of the") +:+. phrase intrslce, ch scalFunc,
  S "is typically either a half-sine along the", phrase slpSrf `sC`
  S "or a constant"]

--
--
mobShearWO :: RelationConcept
mobShearWO = makeRC "mobShearWO"
  (nounPhraseSP "mobilized shear force") mobShearWO_desc mobShearWO_rel

mobShearWO_rel :: Relation
mobShearWO_rel = sy shearFNoIntsl $= ((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (cos (inxi baseAngle))

mobShearWO_desc :: Sentence
mobShearWO_desc = foldlSent_ [S "The", phrase assumption,
  S "for the Morgenstern Price", phrase method_, sParen (makeRef2S newA6),
  S "that the", phrase intrslce, phrase shearForce, ch xi,
  S "is proportional to the", phrase intrslce, 
  phrase normForce, ch intNormForce, S "by a proportionality constant",
  ch normToShear, S "and a predetermined scaling function",
  ch scalFunc `sC` S "that changes",
  (S "proportionality as a function" `ofThe`
  S "x-ordinate position of the") +:+. phrase intrslce, ch scalFunc,
  S "is typically either a half-sine along the", phrase slpSrf `sC`
  S "or a constant"]

--

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium")
  momEql_desc momEql_rel -- genDef6Label

momEql_rel :: Relation
momEql_rel = 0 $= momExpr (\ x y -> x -
  (inxi baseWthX / 2 * (inxi intShrForce + inxiM1 intShrForce)) + y)

momEql_desc :: Sentence
momEql_desc = foldlSent_ [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the moment equilibrium to satisfy", makeRef2S equilibrium,
  S "in the direction", phrase perp,
  S "to" +:+. (S "base" +:+ phrase surface `ofThe` phrase slice),
  S "Moment equilibrium is derived from the free body diagram of" +:+.
  (makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section])), S "Index i refers to",
  plural value `ofThe` plural property, S "for", phrase slice :+: S "/" :+:
  plural intrslce, S "following convention in" +:+.
  (makeRef2S $ SRS.physSyst ([]::[Contents]) ([]::[Section])), at_start variable, plural definition,
  S "can be found in", makeRef2S sliceWght, S "to" +:+. makeRef2S lengthLs,
  makeRef2S newA6]
