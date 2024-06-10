{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.DataDefs (dataDefs, intersliceWtrF, angleA, angleB, lengthB,
  lengthLb, lengthLs, slcHeight, normStressDD, tangStressDD, ratioVariation,
  convertFunc1, convertFunc2, nrmForceSumDD, watForceSumDD) where

import Prelude hiding (cos, sin, tan)
import Language.Drasil
import Theory.Drasil (DataDefinition, ddE)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumption)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Quantities.Math as QM (pi_)
import Data.Drasil.Theories.Physics (torqueDD)

import Drasil.SSP.Assumptions (assumpSBSBISL)
import Drasil.SSP.Defs (slice)
import Drasil.SSP.References (chen2005, fredlund1977, karchewski2012, huston2008)
import Drasil.SSP.Unitals (baseAngle, baseLngth, baseWthX, constF, fricAngle,
  fs, genericA, intNormForce, indxn, inx, inxi, inxiM1, midpntHght,
  fn, ft, mobShrC, normToShear, scalFunc, shrResC, slipDist, slipHght, slopeDist,
  slopeHght, surfAngle, surfLngth, totNormStress, tangStress, nrmForceSum,
  watForceSum, sliceHghtRight, sliceHghtLeft, waterHght, waterWeight, watrForce)

----------------------
-- Data Definitions --
----------------------

dataDefs :: [DataDefinition]
dataDefs = [intersliceWtrF, angleA, angleB, lengthB, lengthLb, lengthLs,
  slcHeight, normStressDD, tangStressDD, torqueDD, ratioVariation, convertFunc1,
  convertFunc2, nrmForceSumDD, watForceSumDD, sliceHghtRightDD, sliceHghtLeftDD]

--DD intersliceWtrF: interslice normal water forces

intersliceWtrF :: DataDefinition
intersliceWtrF = ddE intersliceWtrFQD [dRef fredlund1977] Nothing "intersliceWtrF"
  []--Notes
--FIXME: fill empty lists in

intersliceWtrFQD :: SimpleQDef
intersliceWtrFQD = mkQuantDef watrForce intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = completeCase [case1,case2,case3]
  where case1 = (half (square (inxi slopeHght $- inxi slipHght)) `mulRe`
          sy waterWeight `add` (square (inxi waterHght $- inxi slopeHght) `mulRe`
          sy waterWeight), inxi waterHght $>= inxi slopeHght)

        case2 = (half (square (inxi waterHght $- inxi slipHght)) `mulRe` sy waterWeight,
                (inxi slopeHght $> inxi waterHght)
                $&&
                (inxi waterHght $> inxi slipHght))

        case3 = (exactDbl 0, inxi waterHght $<= inxi slipHght)

--DD angleA: base angles

angleA :: DataDefinition
angleA = ddE angleAQD [dRef fredlund1977] Nothing "angleA"
  [angleANotes]
--FIXME: fill empty lists in

angleAQD :: SimpleQDef
angleAQD = mkQuantDef baseAngle angleAEqn

angleAEqn :: Expr
angleAEqn = arctan ((inxi slipHght $- inx slipHght (-1)) $/
  (inxi slipDist $- inx slipDist (-1)))

angleANotes :: Sentence
angleANotes = foldlSent [S "This", phrase equation, S "is based" `S.onThe`
  phrase assumption, S "that the base" `S.ofA` phrase slice,
  S "is a straight line", sParen (refS assumpSBSBISL)]

--DD angleB: surface angles

angleB :: DataDefinition
angleB = ddE angleBQD [dRef fredlund1977] Nothing "angleB"
  [angleBNotes]--Notes
--FIXME: fill empty lists in

angleBQD :: SimpleQDef
angleBQD = mkQuantDef surfAngle angleBEqn

angleBEqn :: Expr
angleBEqn = arctan ((inxi slopeHght $- inx slopeHght (-1)) $/
  (inxi slopeDist $- inx slopeDist (-1)))

angleBNotes :: Sentence
angleBNotes = foldlSent [S "This", phrase equation, S "is based" `S.onThe`
  phrase assumption, S "that the surface" `S.ofA` phrase slice,
  S "is a straight line", sParen (refS assumpSBSBISL)]

--DD lengthB: base width of slices

lengthB :: DataDefinition
lengthB = ddE lengthBQD [dRef fredlund1977] Nothing "lengthB" []--Notes
--FIXME: fill empty lists in

lengthBQD :: SimpleQDef
lengthBQD = mkQuantDef baseWthX lengthBEqn

lengthBEqn :: Expr
lengthBEqn = inxi slipDist $- inx slipDist (-1)

--DD lengthLb: total base lengths of slices

lengthLb :: DataDefinition
lengthLb = ddE lengthLbQD [dRef fredlund1977] Nothing "lengthLb"
  [lengthLbNotes]--Notes
--FIXME: fill empty lists in

lengthLbQD :: SimpleQDef
lengthLbQD = mkQuantDef baseLngth lengthLbEqn

lengthLbEqn :: Expr
lengthLbEqn = inxi baseWthX `mulRe` sec (inxi baseAngle)

lengthLbNotes :: Sentence
lengthLbNotes = foldlSent [baseWthX `definedIn'''`
  lengthB `S.and_` (baseAngle `definedIn'''` angleA)]

--DD lengthLs: surface lengths of slices

lengthLs :: DataDefinition
lengthLs = ddE lengthLsQD [dRef fredlund1977] Nothing "lengthLs"
  [lengthLsNotes]--Notes
--FIXME: fill empty lists in

lengthLsQD :: SimpleQDef
lengthLsQD = mkQuantDef surfLngth lengthLsEqn

lengthLsEqn :: Expr
lengthLsEqn = inxi baseWthX `mulRe` sec (inxi surfAngle)

lengthLsNotes :: Sentence
lengthLsNotes = foldlSent [baseWthX `definedIn'''`
  lengthB `S.and_` (surfAngle `definedIn'''` angleB)]


--DD slcHeight: y-direction heights of slices

slcHeight :: DataDefinition
slcHeight = ddE slcHeightQD [dRef fredlund1977] Nothing "slcHeight"
  slcHeightNotes

slcHeightQD :: SimpleQDef
slcHeightQD = mkQuantDef midpntHght slcHeightEqn

slcHeightEqn :: Expr
slcHeightEqn = oneHalf `mulRe` (inxi sliceHghtRight `add` inxi sliceHghtLeft)

slcHeightNotes :: [Sentence]
slcHeightNotes = [S "This" +:+ phrase equation +:+ S "is based on the" +:+
  phrase assumption +:+ S "that the surface" `S.and_` S "base of a slice" +:+
  S "are straight lines" +:+. sParen (refS assumpSBSBISL),
  ch sliceHghtRight `S.and_` ch sliceHghtLeft +:+ S "are defined in" +:+
  refS sliceHghtRightDD `S.and_` refS sliceHghtLeftDD `sC`
  (S "respectively" !.)]

--DD normStress: total normal stress

normStressDD :: DataDefinition
normStressDD = ddE normStressQD [dRef huston2008] Nothing "normStress" []

normStressQD :: SimpleQDef
normStressQD = mkQuantDef totNormStress normStressEqn

normStressEqn :: Expr
normStressEqn = sy fn $/ sy genericA

--DD tangStress: tangential stress

tangStressDD :: DataDefinition
tangStressDD = ddE tangStressQD [dRef huston2008] Nothing "tangStress" []

tangStressQD :: SimpleQDef
tangStressQD = mkQuantDef tangStress tangStressEqn

tangStressEqn :: Expr
tangStressEqn = sy ft $/ sy genericA

--DD ratioVariation: interslice normal to shear force ratio variation function

ratioVariation :: DataDefinition
ratioVariation = ddE ratioVarQD [dRef fredlund1977] Nothing
  "ratioVariation" []

ratioVarQD :: SimpleQDef
ratioVarQD = mkQuantDef scalFunc ratioVarEqn

ratioVarEqn :: Expr
ratioVarEqn = completeCase [case1, case2]
  where case1 = (exactDbl 1, sy constF)

        case2 = (sin (sy QM.pi_ `mulRe` ((inxi slipDist $- idx (sy slipDist) (int 0)) $/
                (indxn slipDist $- idx (sy slipDist) (int 0)))), not_ (sy constF))

--DD convertFunc1: first function for incorporating interslice forces into shear force

convertFunc1 :: DataDefinition
convertFunc1 = ddE convertFunc1QD (map dRef [chen2005, karchewski2012]) Nothing
  "convertFunc1" [convertFunc1Notes]

convertFunc1QD :: SimpleQDef
convertFunc1QD = mkQuantDef shrResC convertFunc1Eqn

convertFunc1Eqn :: Expr
convertFunc1Eqn = (sy normToShear `mulRe` inxi scalFunc `mulRe`
  cos (inxi baseAngle) $- sin (inxi baseAngle)) `mulRe` tan (sy fricAngle) $-
  ((sy normToShear `mulRe` inxi scalFunc `mulRe` sin (inxi baseAngle) `add`
  cos (inxi baseAngle)) `mulRe` sy fs)

convertFunc1Notes :: Sentence
convertFunc1Notes = foldlSent [scalFunc `definedIn'''` ratioVariation `S.and_` (baseAngle `definedIn'''` angleA)]

--DD convertFunc2: second function for incorporating interslice forces into shear force

convertFunc2 :: DataDefinition
convertFunc2 = ddE convertFunc2QD (map dRef [chen2005, karchewski2012]) Nothing
  "convertFunc2" [convertFunc2Notes]

convertFunc2QD :: SimpleQDef
convertFunc2QD = mkQuantDef mobShrC convertFunc2Eqn

convertFunc2Eqn :: Expr
convertFunc2Eqn = ((sy normToShear `mulRe` inxi scalFunc `mulRe`
  cos (inxi baseAngle) $- sin (inxi baseAngle)) `mulRe` tan (sy fricAngle) $-
  ((sy normToShear `mulRe` inxi scalFunc `mulRe` sin (inxi baseAngle) `add`
  cos (inxi baseAngle)) `mulRe` sy fs)) $/
  inxiM1 shrResC

convertFunc2Notes :: Sentence
convertFunc2Notes = (foldlList Comma List
  [scalFunc `definedIn'''` ratioVariation, baseAngle `definedIn'''` angleA,
  shrResC `definedIn'''` convertFunc1] !.)

{--DD10

resShearWO :: DataDefinition
resShearWO = ddE resShearWOQD [chen2005] resShr_deriv_ssp resShearWOL
  [refS newA3, refS newA4, refS newA5]--Notes
--FIXME: fill empty lists in

resShearWOQD :: QDefinition
resShearWOQD = mkQuantDef shearRNoIntsl resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) `add` (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForceDif) `add` (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) +
  (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) `mulRe` (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) `mulRe` tan (inxi fricAngle) `add` (inxi effCohesion) *
  (inxi baseWthX) `mulRe` sec (inxi baseAngle)

resShr_deriv_ssp :: Derivation
resShr_deriv_ssp = weave [resShrDerivation_sentence, map E resShr_deriv_eqns_ssp]

--DD11

mobShearWO :: DataDefinition
mobShearWO = ddE mobShearWOQD [chen2005] mobShr_deriv_ssp mobShearWOL
  [refS newA3, refS newA4, refS newA5]--Notes
--FIXME: fill empty lists in

mobShearWOQD :: QDefinition
mobShearWOQD = mkQuantDef shearFNoIntsl mobShearWOEqn

mobShearWOEqn :: Expr 
mobShearWOEqn = ((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForceDif) `add` (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) +
  (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) `mulRe` (cos (inxi baseAngle))

mobShr_deriv_ssp :: Derivation
mobShr_deriv_ssp = (weave [mobShrDerivation_sentence, map E mobShr_deriv_eqns_ssp])-}

-----------------
-- Hacks --------
-----------------

nrmForceSumDD, watForceSumDD, sliceHghtRightDD,
  sliceHghtLeftDD :: DataDefinition
nrmForceSumDD = ddE nrmForceSumQD [dRef fredlund1977] Nothing
  "nrmForceSumDD" []--Notes
watForceSumDD = ddE watForceSumQD [dRef fredlund1977] Nothing
  "watForceSumDD" []--Notes
sliceHghtRightDD = ddE sliceHghtRightQD [dRef fredlund1977] Nothing
  "sliceHghtRightDD" []--Notes
sliceHghtLeftDD = ddE sliceHghtLeftQD [dRef fredlund1977] Nothing
  "sliceHghtLeftDD" []--Notes

nrmForceSumQD :: SimpleQDef
nrmForceSumQD = ec nrmForceSum (inxi intNormForce `add` inxiM1 intNormForce)

watForceSumQD :: SimpleQDef
watForceSumQD = ec watForceSum (inxi watrForce `add` inxiM1 watrForce)

sliceHghtRightQD :: SimpleQDef
sliceHghtRightQD = ec sliceHghtRight (inxi slopeHght $- inxi slipHght)

sliceHghtLeftQD :: SimpleQDef
sliceHghtLeftQD = ec sliceHghtLeft (inxiM1 slopeHght $- inxiM1 slipHght)

--------------------------
-- Derivation Sentences --
--------------------------

-- FIXME: move derivations with the appropriate data definition

{-resShr_deriv_sentences_ssp_s1 :: [Sentence]
resShr_deriv_sentences_ssp_s1 = [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", ch shrResI, S "in" +:+. refS genDef3Label, S "The",
  phrase nrmFSubWat, S "in the", phrase equation, S "for", ch shrResI,
  S "of the soil is defined in the perpendicular force equilibrium",
  S "of a slice from", makeRefS genDef2Label `sC` S "using the", getTandS nrmFSubWat,
  S "of", refS effStress, S "shown in", eqN 1]

resShr_deriv_sentences_ssp_s2 :: [Sentence]
resShr_deriv_sentences_ssp_s2 = [plural value `the_ofThe'` S "interslice forces",
  ch intNormForce `S.and_` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  refS sliceWght, S "to" +:+. refS lengthLs,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", ch nrmFNoIntsl, S "in", eqN 2]

resShr_deriv_sentences_ssp_s3 :: [Sentence]
resShr_deriv_sentences_ssp_s3 = [S "Using", ch nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  plural value, S "as done in", eqN 3]

resShr_deriv_sentences_ssp_s4 :: [Sentence]
resShr_deriv_sentences_ssp_s4 = [S "This can be further simplified by considering assumptions",
  refS newA10, S "and", refS newA12 `sC`
  S "which state that the seismic coefficient and the external force" `sC` S "respectively"
  `sC` S "are0", S "Removing seismic and external forces yields ", eqN 4]

resShrDerivation_sentence :: [Sentence]
resShrDerivation_sentence = map foldlSentCol [resShr_deriv_sentences_ssp_s1, resShr_deriv_sentences_ssp_s2,
  resShr_deriv_sentences_ssp_s3, resShr_deriv_sentences_ssp_s4]

resShr_deriv_eqns_ssp :: [Expr]
resShr_deriv_eqns_ssp = [eq1, eq2, eq3, eq8]

eq1, eq2, eq3, eq8 :: Expr
eq1 = (inxi nrmFSubWat) $= eqlExpr cos sin (\x y -> x -
  inxiM1 intShrForce `add` inxi intShrForce `add` y) - inxi baseHydroForce

eq2 = (inxi nrmFNoIntsl) $= (((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) `add` (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForce) `add` (inxiM1 watrForce) `add` (inxi surfHydroForce) *
  sin (inxi surfAngle) `add` (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (inxi baseHydroForce))

eq3 = inxi shearRNoIntsl $= (inxi nrmFNoIntsl) `mulRe` tan (inxi fricAngle) +
  (inxi effCohesion) `mulRe` (inxi baseWthX) `mulRe` sec (inxi baseAngle) $=
  (((inxi slcWght) `add` (inxi surfHydroForce) `mulRe` (cos (inxi surfAngle)) +
  (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) `mulRe` (cos (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) - (inxi watrForceDif) +
  (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) `add` (inxi surfLoad) *
  (sin (inxi impLoadAngle))) `mulRe` (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) `mulRe` tan (inxi fricAngle) `add` (inxi effCohesion) *
  (inxi baseWthX) `mulRe` sec (inxi baseAngle)

eq8 = inxi shearRNoIntsl $=
  (((inxi slcWght) `add` (inxi surfHydroForce) `mulRe` (cos (inxi surfAngle))) `mulRe` (cos (inxi baseAngle)) +
  (- (inxi watrForceDif) +
  (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) `add` (inxi surfLoad) *
  (sin (inxi impLoadAngle))) `mulRe` (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) `mulRe` tan (inxi fricAngle) `add` (inxi effCohesion) *
  (inxi baseWthX) `mulRe` sec (inxi baseAngle)

-------old chunk---------

resShrDerivation :: [Contents]
resShrDerivation = [

  foldlSP [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", ch shrResI, S "in" +:+. refS genDef3Label, S "The",
  phrase nrmFSubWat, S "in the", phrase equation, S "for", ch shrResI,
  S "of the soil is defined in the perpendicular force equilibrium",
  S "of a slice from", makeRefS bsShrFEq `sC` S "using the", getTandS nrmFSubWat,
  S "of", refS effStress, S "shown in", eqN 5],
  
  eqUnR' $ (inxi nrmFSubWat) $= eqlExpr cos sin (\x y -> x -
  inxiM1 intShrForce `add` inxi intShrForce `add` y) - inxi baseHydroForce,
  
  foldlSP [plural value `the_ofThe'` S "interslice forces",
  ch intNormForce `S.and_` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  refS sliceWght, S "to" +:+. refS lengthLs,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", ch nrmFNoIntsl, S "in", eqN 2],

  eqUnR' $
  (inxi nrmFNoIntsl) $= (((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) `add` (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForce) `add` (inxiM1 watrForce) `add` (inxi surfHydroForce) *
  sin (inxi surfAngle) `add` (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (inxi baseHydroForce)),
  
  foldlSP [S "Using", ch nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  plural value, S "as done in", eqN 3],
  
  eqUnR' $
  inxi shearRNoIntsl $= (inxi nrmFNoIntsl) `mulRe` tan (inxi fricAngle) +
  (inxi effCohesion) `mulRe` (inxi baseWthX) `mulRe` sec (inxi baseAngle) $=
  (((inxi slcWght) `add` (inxi surfHydroForce) `mulRe` (cos (inxi surfAngle)) +
  (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) `mulRe` (cos (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) - (inxi watrForceDif) +
  (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) `add` (inxi surfLoad) *
  (sin (inxi impLoadAngle))) `mulRe` (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) `mulRe` tan (inxi fricAngle) `add` (inxi effCohesion) *
  (inxi baseWthX) `mulRe` sec (inxi baseAngle)

  ]

------------------------------------------------------------------

mobShr_deriv_sentences_ssp_s1 :: [Sentence]
mobShr_deriv_sentences_ssp_s1 = [S "The", phrase mobShrI, S "acting on a slice is defined as",
  ch mobShrI, S "from the force equilibrium in", refS genDef2Label `sC`
  S "also shown in", eqN 5]

mobShr_deriv_sentences_ssp_s2 :: [Sentence]
mobShr_deriv_sentences_ssp_s2 = [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce,
  S "Consider a force equilibrium", S wiif `sC` S "to obtain the",
  getTandS shearFNoIntsl `sC` S "as done in", eqN 6]

mobShr_deriv_sentences_ssp_s3 :: [Sentence]
mobShr_deriv_sentences_ssp_s3 = [S "The" +:+ plural value +:+ S "of" +:+ 
  ch shearFNoIntsl +:+ S "is now defined completely in terms of the" +:+
  S "known" +:+. plural value +:+ S "This can be further simplified by considering assumptions" +:+
  refS newA10 +:+ S "and" +:+ refS newA12 `sC`
  S "which state that the seismic coefficient and the external force" `sC` S "respectively"
  `sC` S "are0" +:+ S "Removing seismic and external forces yields " +:+ eqN 7]


mobShrDerivation_sentence :: [Sentence]
mobShrDerivation_sentence = map foldlSentCol [mobShr_deriv_sentences_ssp_s1, mobShr_deriv_sentences_ssp_s2,
  mobShr_deriv_sentences_ssp_s3]

mobShr_deriv_eqns_ssp :: [Expr]
mobShr_deriv_eqns_ssp = [eq4, eq5, eq6]

eq4, eq5, eq6:: Expr
eq4 = inxi mobShrI $= eqlExpr sin cos
    (\x y -> x - inxiM1 intShrForce `add` inxi intShrForce `add` y)

eq5 = inxi shearFNoIntsl $= ((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForceDif) `add` (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) +
  (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) `mulRe` (cos (inxi baseAngle))

eq6 = inxi shearFNoIntsl $= ((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle))) *
  (sin (inxi baseAngle)) -
  ((inxi watrForceDif) `add` (inxi surfHydroForce) `mulRe` sin (inxi surfAngle)) `mulRe` (cos (inxi baseAngle))

  ------old chunk-----
mobShrDerivation :: [Contents]
mobShrDerivation = [

  foldlSP [S "The", phrase mobShrI, S "acting on a slice is defined as",
  ch mobShrI, S "from the force equilibrium in", refS genDef2Label `sC`
  S "also shown in", eqN 4],
  
  eqUnR' $ inxi mobShrI $= eqlExpr sin cos
    (\x y -> x - inxiM1 intShrForce `add` inxi intShrForce `add` y),
  
  foldlSP [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce,
  S "Consider a force equilibrium", S wiif `sC` S "to obtain the",
  getTandS shearFNoIntsl `sC` S "as done in", eqN 5],
  
  eqUnR' $
  inxi shearFNoIntsl $= ((inxi slcWght) `add` (inxi surfHydroForce) *
  (cos (inxi surfAngle)) `add` (inxi surfLoad) `mulRe` (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) `mulRe` (inxi slcWght) -
  (inxi watrForceDif) `add` (inxi surfHydroForce) `mulRe` sin (inxi surfAngle) +
  (inxi surfLoad) `mulRe` (sin (inxi impLoadAngle))) `mulRe` (cos (inxi baseAngle)),
  
  foldlSP [S "The", plural value, S "of", ch shearRNoIntsl `S.and_`
  ch shearFNoIntsl, S "are now defined completely in terms of the",
  S "known force property", plural value, S "of", refS sliceWght, S "to", 
  refS lengthLs]

  ]-}
