module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, generalDefinitions) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.DocLang.SRS as SRS (physSystLabel)

import Data.Drasil.Concepts.Documentation (assumption, definition, 
  method_, property, system, value, variable)
import Data.Drasil.Concepts.Math (normal, perp, surface)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)

import Data.Drasil.Quantities.Physics (displacement, force)
import Data.Drasil.Quantities.SolidMechanics (nrmStrss)

import Data.Drasil.SentenceStructures (foldlSent, getTandS, ofThe, sAnd)

import Drasil.SSP.Assumptions (newA5)
import Drasil.SSP.BasicExprs (eqlExpr, momExpr)
import Drasil.SSP.DataDefs (lengthLs, sliceWght)
import Drasil.SSP.Defs (intrslce, slice, slope, slpSrf)
import Drasil.SSP.Labels (genDef1Label, genDef2Label, genDef3Label, genDef4Label, 
  genDef5Label, genDef6Label)
import Drasil.SSP.References (chen2005)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, fricAngle, fs, index, intNormForce, intShrForce, inxi, inxiM1, 
  mobShrI, normToShear, nrmFSubWat, scalFunc, shearFNoIntsl, shrResI, 
  shrResI, shrStress, totNrmForce, xi)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [
  gd'' normForcEq  [makeRef chen2005]   "normForcEq"  [nmFEq_desc],
  gd'' bsShrFEq    [makeRef chen2005]   "bsShrFEq"    [bShFEq_desc],
  gd'' resShr      [makeRef chen2005]   "resShr"      [resShr_desc],
  gd'' mobShr      [makeRef chen2005]   "mobShr"      [mobShr_desc],
  gd'' normShrR    [makeRef chen2005]   "normShrR"    [nmShrR_desc],
  gd'' momentEql   [makeRef chen2005]   "momentEql"   [momEql_desc]
  ]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEq_desc nmFEq_rel genDef1Label

nmFEq_rel :: Relation
nmFEq_rel = inxi totNrmForce $= eqlExpr cos sin
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

nmFEq_desc :: Sentence
nmFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", makeRefS equilibrium, S "in the direction",
  phrase perp, S "to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for", (phrase normForce `ofThe`
  phrase surface) +:+. ch totNrmForce, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRefS SRS.physSystLabel, S "Index i",
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRefS SRS.physSystLabel, at_start force, phrase variable,
  plural definition, S "can be found in", makeRefS sliceWght, S "to",
  makeRefS lengthLs]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEq_desc bShFEq_rel genDef2Label

bShFEq_rel :: Relation
bShFEq_rel = inxi mobShrI $= eqlExpr sin cos
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

bShFEq_desc :: Sentence
bShFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", makeRefS equilibrium, S "in the direction",
  S "parallel to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for the", phrase shearForce,
  S "on the base" +:+. ch mobShrI, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRefS SRS.physSystLabel, S "Index", ch index,
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRefS SRS.physSystLabel, at_start force, phrase variable,
  plural definition, S "can be found in", makeRefS sliceWght, S "to",
  makeRefS lengthLs]

--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat * tan (inxi fricAngle) + inxi cohesion *
  inxi baseWthX * sec (inxi baseAngle)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShr_desc resShr_rel genDef3Label

resShr_rel :: Relation
resShr_rel = inxi shrResI $= shrResEqn

resShr_desc :: Sentence
resShr_desc = foldlSent [S "The Mohr-Coulomb resistive shear strength of a",
  phrase slice, ch shrStress, S "from", makeRefS mcShrStrgth,
  S "is multiplied by the area", E $ sy baseWthX * sec(sy baseAngle) * 1,
  S "to obtain the" +:+. getTandS shrResI, S "Note the extra", E 1,
  S "is to represent a unit of width which is multiplied by the",
  getTandS baseLngth, S "of the plane where the", phrase normal,
  S "occurs, where", (E $ sy baseLngth $= sy baseWthX * sec(sy baseAngle))
  `sAnd` ch baseWthX, S "is the x width of the base. This accounts for the",
  phrase nrmFSubWat, E $ sy nrmFSubWat $= sy totNrmForce - sy baseHydroForce,
  S "of a soil from", -- FIXME: add prime to nrmStrss
  makeRefS effStress, S "where the", phrase nrmStrss,
  S "is multiplied by the same area to obtain the", phrase nrmFSubWat,
  E $ sy nrmStrss * sy baseWthX * sec(sy baseAngle) * 1 $= sy nrmFSubWat]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobile shear force") mobShr_desc mobShr_rel genDef4Label

mobShr_rel :: Relation
mobShr_rel = inxi mobShrI $= inxi shrResI / sy fs $= shrResEqn / sy fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent [
  S "From", phrase definition `ofThe` phrase fs, S "in", makeRefS factOfSafety `sC`
  S "and the new", phrase definition, S "of", ch shrResI `sC` S "a new",
  S "relation for", S "net mobile" +:+ phrase shearForce `ofThe` phrase slice,
  ch shearFNoIntsl, S "is found as the resistive shear", ch shrResI,
  sParen (makeRefS genDef3Label), S "divided by the factor of safety", ch fs]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR"
  (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel genDef5Label

nmShrR_rel :: Relation
nmShrR_rel = sy intShrForce $= sy normToShear * sy scalFunc * sy intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent [S "The", phrase assumption,
  S "for the Morgenstern Price", phrase method_, sParen (makeRefS newA5),
  S "that the", phrase intrslce, phrase shearForce, ch xi,
  S "is proportional to the", phrase intrslce, 
  phrase normForce, ch intNormForce, S "by a proportionality constant",
  ch normToShear, S "and a predetermined scaling function",
  ch scalFunc `sC` S "that changes",
  (S "proportionality as a function" `ofThe`
  S "x-ordinate position of the") +:+. phrase intrslce, ch scalFunc,
  S "is typically either a half-sine along the", phrase slpSrf `sC`
  S "or a constant"]

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium")
  momEql_desc momEql_rel genDef6Label

momEql_rel :: Relation
momEql_rel = 0 $= momExpr (\ x y -> x -
  (inxi baseWthX / 2 * (inxi intShrForce + inxiM1 intShrForce)) + y)

momEql_desc :: Sentence
momEql_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the moment equilibrium to satisfy", makeRefS equilibrium,
  S "in the direction", phrase perp,
  S "to" +:+. (S "base" +:+ phrase surface `ofThe` phrase slice),
  S "Moment equilibrium is derived from the free body diagram of" +:+.
  makeRefS SRS.physSystLabel, S "Index i refers to",
  plural value `ofThe` plural property, S "for", phrase slice :+: S "/" :+:
  plural intrslce, S "following convention in" +:+.
  makeRefS SRS.physSystLabel, at_start variable, plural definition,
  S "can be found in", makeRefS sliceWght, S "to", makeRefS lengthLs]
