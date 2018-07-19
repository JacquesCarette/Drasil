module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, netForcex, netForcey, hookesLaw2d, displVect, generalDefinitions) where

import Prelude hiding (sin, cos, tan)
import Language.Drasil
import Drasil.DocLang (refA)

import Drasil.SSP.Assumptions (newA5, sspRefDB)
import Drasil.SSP.BasicExprs (displMtx, eqlExpr, momExpr, rotMtx)
import Drasil.SSP.DataDefs (ddRef, lengthLb, lengthLs, mobShearWO, sliceWght)
import Drasil.SSP.Defs (intrslce, slice, slope, slpSrf)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, dx_i, dy_i, earthqkLoadFctr, elmNrmDispl, elmPrllDispl, fricAngle, 
  fs, fx, fy, genDisplace, genPressure, impLoadAngle, index, intNormForce, 
  intShrForce, inxi, inxiM1, mobShrI, normToShear, nrmDispl, nrmFSubWat, 
  nrmStiffBase, rotatedDispl, scalFunc, shearFNoIntsl, shrDispl, shrResI, 
  shrResI, shrStiffIntsl, shrStress, slcWght, surfAngle, surfHydroForce, 
  surfLoad, totNrmForce, watrForceDif, xi)

import Data.Drasil.Concepts.Documentation (assumption, definition, element, 
  method_, model, property, system, value, variable)
import Data.Drasil.Concepts.Math (angle, matrix, normal, perp, surface, vector)
import Data.Drasil.Concepts.PhysicalProperties (len, mass)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)

import Data.Drasil.Quantities.Physics (displacement, force)
import Data.Drasil.Quantities.SolidMechanics (nrmStrss)

import Data.Drasil.SentenceStructures (acroGD, acroT, foldlSent, getTandS, 
  isThe, ofThe, sAnd)

import Drasil.DocLang.SRS as SRS (physSystLabel)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [gd' normForcEq (Nothing :: Maybe DerUChunk) ([] :: Derivation) "normForcEq" [nmFEq_desc],
  gd' bsShrFEq (Nothing :: Maybe DerUChunk) ([] :: Derivation) "bsShrFEq" [bShFEq_desc],
  gd' resShr (Nothing :: Maybe DerUChunk) ([] :: Derivation) "resShr" [resShr_desc],
  gd' mobShr (Nothing :: Maybe DerUChunk) ([] :: Derivation) "mobShr" [mobShr_desc],
  gd' normShrR (Nothing :: Maybe DerUChunk) ([] :: Derivation) "normShrR" [nmShrR_desc],
  gd' momentEql (Nothing :: Maybe DerUChunk) ([] :: Derivation) "momentEql" [momEql_desc],
  gd netForcex (Nothing :: Maybe DerUChunk) ([] :: Derivation) (mkLabelRA'' "netForcex"),
  gd' netForcey (Nothing :: Maybe DerUChunk) ([] :: Derivation) "netForcey" [fNet_desc],
  gd' hookesLaw2d (Nothing :: Maybe DerUChunk) ([] :: Derivation) "hookesLaw2d" [hooke2d_desc],
  gd' displVect (Nothing :: Maybe DerUChunk) ([] :: Derivation) "displVect" [disVec_desc]]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEq_desc nmFEq_rel Nothing --label 

nmFEq_rel :: Relation
nmFEq_rel = inxi totNrmForce $= eqlExpr cos sin
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

nmFEq_desc :: Sentence
nmFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", acroT 2, S "in the direction",
  phrase perp, S "to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for", (phrase normForce `ofThe`
  phrase surface) +:+. ch totNrmForce, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRef SRS.physSystLabel, S "Index i",
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRef SRS.physSystLabel, at_start force, phrase variable,
  plural definition, S "can be found in", ddRef sliceWght, S "to",
  ddRef lengthLs]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEq_desc bShFEq_rel Nothing --label 

bShFEq_rel :: Relation
bShFEq_rel = inxi mobShrI $= eqlExpr sin cos
  (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

bShFEq_desc :: Sentence
bShFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", acroT 2, S "in the direction",
  S "parallel to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for the", phrase shearForce,
  S "on the base" +:+. ch mobShrI, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRef SRS.physSystLabel, S "Index", ch index,
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRef SRS.physSystLabel, at_start force, phrase variable,
  plural definition, S "can be found in", ddRef sliceWght, S "to",
  ddRef lengthLs]

--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat * tan (inxi fricAngle) + inxi cohesion *
  inxi baseWthX * sec (inxi baseAngle)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShr_desc resShr_rel Nothing --label 

resShr_rel :: Relation
resShr_rel = inxi shrResI $= shrResEqn

resShr_desc :: Sentence
resShr_desc = foldlSent [S "The Mohr-Coulomb resistive shear strength of a",
  phrase slice, ch shrStress, S "from", acroT 3,
  S "is multiplied by the area", E $ sy baseWthX * sec(sy baseAngle) * 1,
  S "to obtain the" +:+. getTandS shrResI, S "Note the extra", E 1,
  S "is to represent a unit of width which is multiplied by the",
  getTandS baseLngth, S "of the plane where the", phrase normal,
  S "occurs, where", (E $ sy baseLngth $= sy baseWthX * sec(sy baseAngle))
  `sAnd` ch baseWthX, S "is the x width of the base. This accounts for the",
  phrase nrmFSubWat, E $ sy nrmFSubWat $= sy totNrmForce - sy baseHydroForce,
  S "of a soil from", -- FIXME: add prime to nrmStrss
  acroT 4, S "where the", phrase nrmStrss,
  S "is multiplied by the same area to obtain the", phrase nrmFSubWat,
  E $ sy nrmStrss * sy baseWthX * sec(sy baseAngle) * 1 $= sy nrmFSubWat]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobile shear force") mobShr_desc mobShr_rel
  Nothing --label 

mobShr_rel :: Relation
mobShr_rel = inxi mobShrI $= inxi shrResI / sy fs $= shrResEqn / sy fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent [
  S "From", phrase definition `ofThe` phrase fs, S "in", acroT 1 `sC`
  S "and the new", phrase definition, S "of", ch shrResI `sC` S "a new",
  S "relation for", S "net mobile" +:+ phrase shearForce `ofThe` phrase slice,
  ch shearFNoIntsl, S "is found as the resistive shear", ch shrResI,
  sParen (acroGD 3), S "divided by the factor of safety", ch fs]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR"
  (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel 
  Nothing --label

nmShrR_rel :: Relation
nmShrR_rel = sy intShrForce $= sy normToShear * sy scalFunc * sy intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent [S "The", phrase assumption,
  S "for the Morgenstern Price", phrase method_, sParen (refA sspRefDB newA5),
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
  momEql_desc momEql_rel Nothing --label 

momEql_rel :: Relation
momEql_rel = 0 $= momExpr (\ x y -> x -
  (inxi baseWthX / 2 * (inxi intShrForce + inxiM1 intShrForce)) + y)

momEql_desc :: Sentence
momEql_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the moment equilibrium to satisfy", acroT 2,
  S "in the direction", phrase perp,
  S "to" +:+. (S "base" +:+ phrase surface `ofThe` phrase slice),
  S "Moment equilibrium is derived from the free body diagram of" +:+.
  makeRef SRS.physSystLabel, S "Index i refers to",
  plural value `ofThe` plural property, S "for", phrase slice :+: S "/" :+:
  plural intrslce, S "following convention in" +:+.
  makeRef SRS.physSystLabel, at_start variable, plural definition,
  S "can be found in", ddRef sliceWght, S "to", ddRef lengthLs]

--
netForcex :: RelationConcept
netForcex = makeRC "netForce" (nounPhraseSP "net x-component force")
  EmptyS fNetx_rel Nothing --label 

fNetx_rel :: Relation
fNetx_rel = inxi fx $= (negate $ inxi watrForceDif) -
  (sy earthqkLoadFctr)*(inxi slcWght)
  - (inxi baseHydroForce) * sin (inxi baseAngle) +
  (inxi surfHydroForce) * sin (inxi surfAngle)
  + (inxi surfLoad) * sin (inxi impLoadAngle)

netForcey :: RelationConcept
netForcey = makeRC "netForce" (nounPhraseSP "net y-component force")
  fNet_desc fNety_rel Nothing --label

fNety_rel :: Relation
fNety_rel = inxi fy $= (negate $ inxi slcWght) +
  (inxi baseHydroForce) * cos (inxi baseAngle)
  - (inxi surfHydroForce) * cos (inxi surfAngle) -
  (inxi surfLoad) * cos (inxi impLoadAngle)


fNet_desc :: Sentence
fNet_desc = foldlSent [S "These equations show the net sum of the",
  plural force, S "acting on a", phrase slice, 
  S "for the RFEM", phrase model, S "and the", plural force,
  S "that create an applied load on the" +:+. phrase slice, ch fx,
  S "refers to the load in the direction", phrase perp, S "to the",
  S "direction of the", phrase force, S "of gravity for", phrase slice,
  ch index `sC` S "while", ch fy, S "refers to the load in the",
  S "direction parallel to the", phrase force, S "of gravity for", 
  phrase slice +:+. ch index, at_start' force, 
  S "are found in the free body diagram of" +:+.
  makeRef SRS.physSystLabel, S "In this", phrase model,
  --FIXME: hacked link
  S "the", plural element, S "are not exerting", plural force,
  S "on each other" `sC` S "so the", phrase intrslce, plural force,
  ch intNormForce, S "and", ch intShrForce, S "are not a part of the"
  +:+. phrase model, S "Index", ch index, 
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following", 
  S "convention in" +:+. makeRef SRS.physSystLabel,
  at_start force, phrase variable, plural definition, S "can be found in",
  ddRef sliceWght, S "to", ddRef lengthLb]

--
hookesLaw2d :: RelationConcept
hookesLaw2d = makeRC "hookesLaw2d" (nounPhraseSP "Hooke's law 2D")
  hooke2d_desc hooke2d_rel Nothing --label 

hooke2d_rel :: Relation
hooke2d_rel = vec2D (inxi genPressure) (inxi genPressure) $=
  dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase) *
  vec2D (inxi dx_i) (inxi dy_i)

hooke2d_desc :: Sentence
hooke2d_desc = foldlSent [
  S "A 2D component implementation of Hooke's law as seen in" +:+.
  acroT 5, ch elmPrllDispl, S "is", phrase displacement `ofThe`
  phrase element, S "normal to the", phrase surface, S "and",
  ch elmNrmDispl, S "is", phrase displacement `ofThe` phrase element,
  S "parallel to the" +:+. phrase surface, S "Pn,i",
  S "is the net pressure acting normal to the", phrase surface `sC`
  S "and", S "Pt,i", S "is the net pressure acting parallel to the" +:+.
  phrase surface, S "Pressure is used in place of", phrase force,
  S "as the", phrase surface, S "has not been normalized for it's" +:+.
  phrase len, S "The stiffness", plural value, S "Kn,i" `sAnd` S "Kt,i",
  -- FIXME: Pn,i ~ Pt,i ~ Kn,i ~ Kt,i need symbols 
  S "are then the resistance to", phrase displacement,
  S "in the respective directions defined as in" +:+. ddRef mobShearWO,
  S "The pressure", plural force, S "would be the result of applied",
  S "loads on the", phrase mass `sC` S "the product of the stiffness",
  plural element, S "with the", phrase displacement, S "would be the",
  phrase's mass, S "reactive", phrase force, S "that creates equilibrium",
  S "with the applied", plural force, S "after reaching the equilibrium",
  phrase displacement]


--
displVect :: RelationConcept
displVect = makeRC "displVect" (nounPhraseSP "displacement vectors")
  disVec_desc disVec_rel Nothing --label

disVec_rel :: Relation
disVec_rel = inxi rotatedDispl $= vec2D (inxi shrDispl) (inxi nrmDispl) $=
  rotMtx * (inxi genDisplace) $= rotMtx * displMtx

disVec_desc :: Sentence
disVec_desc = foldlSent [at_start' vector, S "describing the",
  phrase displacement, S "of", phrase slice +:+. ch index,
  ch genDisplace `isThe` phrase displacement,
  S "in the unrotated coordinate system" `sC` S "where",
  ch dx_i `isThe` phrase displacement, S "of the", phrase slice,
  phrase perp, S "to the direction of gravity, and", ch dy_i `isThe`
  phrase displacement, S "of the", phrase slice, S "parallel to the", 
  phrase force +:+. S "of gravity", ch rotatedDispl `isThe`
  phrase displacement, S "in the rotated coordinate", phrase system `sC`
  S "where", ch shrDispl `isThe` phrase displacement, S "of the",
  phrase slice, S "parallel to the", phrase slice, S "base, and", 
  ch dy_i `isThe` phrase displacement, S "of the", phrase slice,
  phrase perp, S "to the", phrase slice +:+. S "base", ch rotatedDispl,
  S "can also be found by rotating", ch genDisplace,
  S "clockwise by the base", phrase angle `sC` ch baseAngle,
  S "through a rotation", phrase matrix, S "as shown"]
  --FIXME: some symbols need to be vectors
