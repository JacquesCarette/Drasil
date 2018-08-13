module Drasil.SSP.DataDefs (dataDefns, soilStiffness, netFDsplcmntEqbm, 
  mobShearWO, displcmntRxnF, intrsliceF, surfLoads, seismicLoadF, 
  resShearWO, lengthLs, lengthLb, sliceWght, ddRef, fixme1, fixme2,
  stfMtrxDerivation, mobShrDerivation, resShrDerivation) where 

import Prelude hiding (cos, sin, tan)
import Language.Drasil
import Drasil.DocLang (ModelDB, ddRefDB, mdb, refDD)
import Control.Lens ((^.))

import Drasil.SSP.BasicExprs (displMtx, eqlExpr, rotMtx)
import Drasil.SSP.Defs (intrslce)
import Drasil.SSP.Labels (genDef2Label, genDef3Label, genDef8Label, genDef9Label)
import Drasil.SSP.TMods (effStress_new)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, constant_A, constant_K, constant_a, dryWeight, earthqkLoadFctr, 
  effStiffA, effStiffB, fricAngle, genDisplace, genForce, genPressure, 
  impLoadAngle, index, intNormForce, intShrForce, inx, inxi, inxiM1, mobShrI, 
  normStress, normToShear, nrmDispl, nrmFNoIntsl, nrmFSubWat, nrmStiffBase, 
  nrmStiffIntsl, poissnsRatio, rotatedDispl, satWeight, scalFunc, shearFNoIntsl, 
  shearRNoIntsl, shrDispl, shrResI, shrStiffBase, shrStiffIntsl, slcWght, 
  slipDist, slipHght, slopeDist, slopeHght, surfAngle, surfHydroForce, surfLngth, 
  surfLoad, ufixme1, ufixme2, waterHght, waterWeight, watrForce, watrForceDif, wiif)

import Data.Drasil.Quantities.SolidMechanics as SM (poissnsR)
import Data.Drasil.Utils (eqUnR', weave)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (definition, element, value)
import Data.Drasil.SentenceStructures (eqN, foldlSentCol, foldlSP, getTandS, 
  isThe, ofThe, ofThe', sAnd, sOf)
import Data.Drasil.Concepts.Math (angle, equation)

------------------------
--  Data Definitions  --
------------------------
ddRef :: QDefinition -> Sentence
ddRef = refDD (ddRefDB sspRefMDB) 

sspRefMDB :: ModelDB
sspRefMDB = mdb [] [] sspDataDefs [] 

--FIXME:should be deleted eventually?
sspDataDefs :: [QDefinition]
sspDataDefs = [sliceWght, baseWtrF, surfWtrF, intersliceWtrF, angleA, angleB,
  lengthB, lengthLb, lengthLs, seismicLoadF, surfLoads, intrsliceF, resShearWO,
  mobShearWO, displcmntRxnF, displcmntBasel, netFDsplcmntEqbm, shearStiffness,
  soilStiffness, fixme1, fixme2]

dataDefns :: [DataDefinition]
dataDefns = [sliceWght_new, baseWtrF_new, surfWtrF_new, intersliceWtrF_new,
  angleA_new, angleB_new, lengthB_new, lengthLb_new, lengthLs_new, seismicLoadF_new,
  surfLoads_new, intrsliceF_new, resShearWO_new, mobShearWO_new, displcmntRxnF_new,
  displcmntBasel_new, netFDsplcmntEqbm_new, shearStiffness_new, soilStiffness_new,
  fixme1_new, fixme2_new]
--FIXME: data definitions need renaming

--DD1

sliceWght_new :: DataDefinition
sliceWght_new = mkDD sliceWght [{-References-}] [{-Derivation-}] "sliceWght" Nothing--Notes
--FIXME: fill empty lists in

sliceWght :: QDefinition
sliceWght = mkQuantDef slcWght slcWgtEqn

slcWgtEqn :: Expr
slcWgtEqn = (inxi baseWthX) * (case_ [case1,case2,case3])
  where case1 = (((inxi slopeHght)-(inxi slipHght ))*(sy satWeight),
          (inxi waterHght) $>= (inxi slopeHght))

        case2 = (((inxi slopeHght)-(inxi waterHght))*(sy dryWeight) +
          ((inxi waterHght)-(inxi slipHght))*(sy satWeight),
          (inxi slopeHght) $> (inxi waterHght) $> (inxi slipHght))

        case3 = (((inxi slopeHght)-(inxi slipHght ))*(sy dryWeight),
          (inxi waterHght) $<= (inxi slipHght))

--DD2

baseWtrF_new :: DataDefinition
baseWtrF_new = mkDD baseWtrF [{-References-}] [{-Derivation-}] "baseWtrF" Nothing--Notes
--FIXME: fill empty lists in

baseWtrF :: QDefinition
baseWtrF = mkQuantDef baseHydroForce bsWtrFEqn 

bsWtrFEqn :: Expr
bsWtrFEqn = (inxi baseLngth)*(case_ [case1,case2])
  where case1 = (((inxi waterHght)-(inxi slipHght))*(sy waterWeight),
          (inxi waterHght) $> (inxi slipHght))

        case2 = (0, (inxi waterHght) $<= (inxi slipHght))

--DD3

surfWtrF_new :: DataDefinition
surfWtrF_new = mkDD surfWtrF [{-References-}] [{-Derivation-}] "surfWtrF" Nothing--Notes
--FIXME: fill empty lists in

surfWtrF :: QDefinition
surfWtrF = mkQuantDef surfHydroForce surfWtrFEqn

surfWtrFEqn :: Expr
surfWtrFEqn = (inxi surfLngth)*(case_ [case1,case2])
  where case1 = (((inxi waterHght)-(inxi slopeHght))*(sy waterWeight),
          (inxi waterHght) $> (inxi slopeHght))

        case2 = (0, (inxi waterHght) $<= (inxi slopeHght))

--DD4

intersliceWtrF_new :: DataDefinition
intersliceWtrF_new = mkDD intersliceWtrF [{-References-}] [{-Derivation-}] "intersliceWtrF" Nothing--Notes
--FIXME: fill empty lists in

intersliceWtrF :: QDefinition
intersliceWtrF = mkQuantDef watrForce intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = case_ [case1,case2,case3]
  where case1 = (((inxi slopeHght)-(inxi slipHght ))$^ 2 / 2  *
          (sy satWeight) + ((inxi waterHght)-(inxi slopeHght))$^ 2 *
          (sy satWeight), (inxi waterHght) $>= (inxi slopeHght))

        case2 = (((inxi waterHght)-(inxi slipHght ))$^ 2 / 2  * (sy satWeight),
                (inxi slopeHght) $> (inxi waterHght) $> (inxi slipHght))

        case3 = (0,(inxi waterHght) $<= (inxi slipHght))

--DD5

angleA_new :: DataDefinition
angleA_new = mkDD angleA [{-References-}] [{-Derivation-}] "angleA" Nothing--Notes
--FIXME: fill empty lists in

angleA :: QDefinition
angleA = mkQuantDef baseAngle angleAEqn

angleAEqn :: Expr
angleAEqn = (inxi slipHght - inx slipHght (-1)) /
  (inxi slipDist - inx slipDist (-1))

--DD5.5

angleB_new :: DataDefinition
angleB_new = mkDD angleB [{-References-}] [{-Derivation-}] "angleB" Nothing--Notes
--FIXME: fill empty lists in

angleB :: QDefinition
angleB = mkQuantDef surfAngle angleBEqn

angleBEqn :: Expr
angleBEqn = (inxi slopeHght - inx slopeHght (-1)) /
  (inxi slopeDist - inx slopeDist (-1))

--DD6

lengthB_new :: DataDefinition
lengthB_new = mkDD lengthB [{-References-}] [{-Derivation-}] "lengthB" Nothing--Notes
--FIXME: fill empty lists in

lengthB :: QDefinition
lengthB = mkQuantDef baseWthX lengthBEqn

lengthBEqn :: Expr
lengthBEqn = inxi slipDist - inx slipDist (-1)

--DD6.3

lengthLb_new :: DataDefinition
lengthLb_new = mkDD lengthLb [{-References-}] [{-Derivation-}] "lengthLb" Nothing--Notes
--FIXME: fill empty lists in

lengthLb :: QDefinition
lengthLb = mkQuantDef baseLngth lengthLbEqn

lengthLbEqn :: Expr
lengthLbEqn = (inxi baseWthX) * sec (inxi baseAngle)

--DD6.6

lengthLs_new :: DataDefinition
lengthLs_new = mkDD lengthLs [{-References-}] [{-Derivation-}] "lengthLs" Nothing--Notes
--FIXME: fill empty lists in

lengthLs :: QDefinition
lengthLs = mkQuantDef surfLngth lengthLsEqn

lengthLsEqn :: Expr
lengthLsEqn = (inxi baseWthX) * sec (inxi surfAngle)

--DD7

seismicLoadF_new :: DataDefinition
seismicLoadF_new = mkDD seismicLoadF [{-References-}] [{-Derivation-}] "seismicLoadF" Nothing--Notes
--FIXME: fill empty lists in

seismicLoadF :: QDefinition
seismicLoadF = mkQuantDef earthqkLoadFctr ssmcLFEqn
  --FIXME: K_E missing for unitals?

ssmcLFEqn :: Expr
ssmcLFEqn = ((sy earthqkLoadFctr) * (inxi slcWght))

--DD8

surfLoads_new :: DataDefinition
surfLoads_new = mkDD surfLoads [{-References-}] [{-Derivation-}] "surfLoads" Nothing--Notes
--FIXME: fill empty lists in

surfLoads :: QDefinition
surfLoads = mkQuantDef surfLoad surfLEqn
  --FIXEME: is this data definition necessary?

surfLEqn :: Expr
surfLEqn = (inxi surfLoad) * (inxi impLoadAngle)
  --FIXME: should be split into two DataDefs

--DD9

intrsliceF_new :: DataDefinition
intrsliceF_new = mkDD intrsliceF [{-References-}] [{-Derivation-}] "intrsliceF" Nothing--Notes
--FIXME: fill empty lists in

intrsliceF :: QDefinition
intrsliceF = mkQuantDef intShrForce intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (sy normToShear) * (inxi scalFunc) * (inxi intNormForce)

--DD10

resShearWO_new :: DataDefinition
resShearWO_new = mkDD resShearWO [{-References-}] resShr_deriv_ssp "resShearWO" Nothing--Notes
--FIXME: fill empty lists in

resShearWO :: QDefinition
resShearWO = mkQuantDef' shearRNoIntsl resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) + (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)

resShr_deriv_ssp :: Derivation
resShr_deriv_ssp = weave [resShrDerivation_sentence, map E resShr_deriv_eqns_ssp]

--DD11

mobShearWO_new :: DataDefinition
mobShearWO_new = mkDD mobShearWO [{-References-}] mobShr_deriv_ssp "mobShearWO" Nothing--Notes
--FIXME: fill empty lists in

mobShearWO :: QDefinition
mobShearWO = mkQuantDef' shearFNoIntsl mobShearWOEqn

mobShearWOEqn :: Expr 
mobShearWOEqn = ((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (cos (inxi baseAngle))

mobShr_deriv_ssp :: Derivation
mobShr_deriv_ssp = (weave [mobShrDerivation_sentence, map E mobShr_deriv_eqns_ssp]) ++
  mobShr_deriv_sentences_ssp_s3

--DD12

displcmntRxnF_new :: DataDefinition
displcmntRxnF_new = mkDD displcmntRxnF [{-References-}] [{-Derivation-}] "displcmntRxnF" Nothing--Notes
--FIXME: fill empty lists in

displcmntRxnF :: QDefinition
displcmntRxnF = mkQuantDef genPressure displcmntRxnFEqn 

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase) * displMtx

--DD12.5

displcmntBasel_new :: DataDefinition
displcmntBasel_new = mkDD displcmntBasel [{-References-}] stfMtrx_deriv_ssp "displcmntBasel" Nothing--Notes
--FIXME: fill empty lists in

displcmntBasel :: QDefinition
displcmntBasel = mkQuantDef' genPressure displcmntBaselEqn

displcmntBaselEqn :: Expr
displcmntBaselEqn = m2x2 (inxi effStiffA) (inxi effStiffB) (inxi effStiffB)
  (inxi effStiffA) * displMtx

stfMtrx_deriv_ssp :: Derivation
stfMtrx_deriv_ssp = [S "Using the force-displacement relationship of" +:+ 
  (makeRef genDef8Label) +:+  S "to define stiffness matrix" +:+ ch shrStiffIntsl `sC`
  S "as seen in" +:+. eqN 6] ++ [(E eq6)] ++ stfMtrx_deriv_sentences_ssp_s1 ++
  stfMtrx_deriv_sentences_ssp_s2 ++ [(E eq7)] ++ stfMtrx_deriv_sentences_ssp_s3
  ++ [(E eq8)] ++ stfMtrx_deriv_sentences_ssp_s4 ++ [(E eq9)] ++ [(E eq10)] ++ [(E eq11)]
  ++ stfMtrx_deriv_sentences_ssp_s5

--DD13

netFDsplcmntEqbm_new :: DataDefinition
netFDsplcmntEqbm_new = mkDD netFDsplcmntEqbm [{-References-}] [{-Derivation-}] "displcmntBasel" Nothing--Notes
--FIXME: fill empty lists in

netFDsplcmntEqbm :: QDefinition
netFDsplcmntEqbm = mkQuantDef genForce netFDsplcmntEqbmEqn 

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = negate (inx surfLngth (-1)) * (inx nrmStiffIntsl (-1)) *
  (inx genDisplace (-1)) + (inx surfLngth (-1) * inx nrmStiffIntsl (-1) +
  inx baseLngth 0 * inx nrmStiffBase 0 + inx surfLngth 0 *
  inx nrmStiffIntsl 0) * (inx genDisplace 0) -
  (inx surfLngth 0) * (inx nrmStiffIntsl 0) * (inx genDisplace 1)

--DD14

shearStiffness_new :: DataDefinition
shearStiffness_new = mkDD shearStiffness [{-References-}] [{-Derivation-}] "shearStiffness" Nothing--Notes
--FIXME: fill empty lists in

shearStiffness :: QDefinition
shearStiffness = mkQuantDef shrStiffBase shearStiffnessEqn  

shearStiffnessEqn :: Expr
shearStiffnessEqn = sy intNormForce / (2 * (1 + sy poissnsRatio)) *
  (dbl 0.1 / sy baseWthX) + (inxi cohesion - sy normStress *
  tan(inxi fricAngle)) / (abs (sy shrDispl) + sy constant_a)

--DD15 this is the second part to the original DD14

soilStiffness_new :: DataDefinition
soilStiffness_new = mkDD soilStiffness [{-References-}] [{-Derivation-}] "soilStiffness" Nothing--Notes
--FIXME: fill empty lists in

soilStiffness :: QDefinition
soilStiffness = mkQuantDef nrmStiffBase soilStiffnessEqn

soilStiffnessEqn :: Expr
soilStiffnessEqn = (case_ [case1,case2])
  where case1 = (block, (sy SM.poissnsR) $< 0)

        case2 = ((dbl 0.01) * block + (sy constant_K) / ((sy nrmDispl)+
          (sy constant_A)), (sy SM.poissnsR) $>= 0)

        block = (sy intNormForce)*(1 - (sy SM.poissnsR))/
          ((1 + (sy SM.poissnsR)) * (1 - 2 *(sy SM.poissnsR) + (sy baseWthX)))

-----------------
-- Hacks --------
-----------------

fixme1_new, fixme2_new :: DataDefinition
fixme1_new = mkDD fixme1 [{-References-}] [{-Derivation-}] "fixme1" Nothing--Notes
fixme2_new = mkDD fixme2 [{-References-}] [{-Derivation-}] "fixme2" Nothing--Notes
--FIXME: fill empty lists in

fixme1 :: QDefinition
fixme1 = ec ufixme1 (inxi intNormForce + inxiM1 intNormForce) (mkLabelSame "fixme1" (Def DD))

fixme2 :: QDefinition
fixme2 = ec ufixme2 (inxi watrForce + inxiM1 watrForce) (mkLabelSame "fixme2" (Def DD))

--------------------------
-- Derivation Sentences --
--------------------------

-- FIXME: move derivations with the appropriate data definition

resShr_deriv_sentences_ssp_s1 :: [Sentence]
resShr_deriv_sentences_ssp_s1 = [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", ch shrResI, S "in" +:+. makeRef genDef3Label, S "The",
  phrase nrmFSubWat, S "in the", phrase equation, S "for", ch shrResI,
  S "of the soil is defined in the perpendicular force equilibrium",
  S "of a slice from", makeRef genDef2Label `sC` S "using the", getTandS nrmFSubWat,
  S "of", makeRef effStress_new, S "shown in", eqN 1]

resShr_deriv_sentences_ssp_s2 :: [Sentence]
resShr_deriv_sentences_ssp_s2 = [plural value `ofThe'` S "interslice forces",
  ch intNormForce `sAnd` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  ddRef sliceWght, S "to" +:+. ddRef lengthLs,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", ch nrmFNoIntsl, S "in", eqN 2]

resShr_deriv_sentences_ssp_s3 :: [Sentence]
resShr_deriv_sentences_ssp_s3 = [S "Using", ch nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  plural value, S "as done in", eqN 3]


resShrDerivation_sentence :: [Sentence]
resShrDerivation_sentence = map foldlSentCol [resShr_deriv_sentences_ssp_s1, resShr_deriv_sentences_ssp_s2,
  resShr_deriv_sentences_ssp_s3]

resShr_deriv_eqns_ssp :: [Expr]
resShr_deriv_eqns_ssp = [eq1, eq2, eq3]

eq1, eq2, eq3 :: Expr
eq1 = (inxi nrmFSubWat) $= eqlExpr cos sin (\x y -> x -
  inxiM1 intShrForce + inxi intShrForce + y) - inxi baseHydroForce

eq2 = (inxi nrmFNoIntsl) $= (((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) + (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForce) + (inxiM1 watrForce) + (inxi surfHydroForce) *
  sin (inxi surfAngle) + (inxi surfLoad) * (sin (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (inxi baseHydroForce))

eq3 = inxi shearRNoIntsl $= (inxi nrmFNoIntsl) * tan (inxi fricAngle) +
  (inxi cohesion) * (inxi baseWthX) * sec (inxi baseAngle) $=
  (((inxi slcWght) + (inxi surfHydroForce) * (cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (cos (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi watrForceDif) +
  (inxi surfHydroForce) * sin (inxi surfAngle) + (inxi surfLoad) *
  (sin (inxi impLoadAngle))) * (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)

-------old chunk---------

resShrDerivation :: [Contents]
resShrDerivation = [

  foldlSP [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", ch shrResI, S "in" +:+. makeRef genDef3Label, S "The",
  phrase nrmFSubWat, S "in the", phrase equation, S "for", ch shrResI,
  S "of the soil is defined in the perpendicular force equilibrium",
  S "of a slice from", makeRef genDef2Label `sC` S "using the", getTandS nrmFSubWat,
  S "of", makeRef effStress_new, S "shown in", eqN 1],
  
  eqUnR' $ (inxi nrmFSubWat) $= eqlExpr cos sin (\x y -> x -
  inxiM1 intShrForce + inxi intShrForce + y) - inxi baseHydroForce,
  
  foldlSP [plural value `ofThe'` S "interslice forces",
  ch intNormForce `sAnd` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  ddRef sliceWght, S "to" +:+. ddRef lengthLs,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", ch nrmFNoIntsl, S "in", eqN 2],

  eqUnR' $
  (inxi nrmFNoIntsl) $= (((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (cos (inxi baseAngle)) + (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForce) + (inxiM1 watrForce) + (inxi surfHydroForce) *
  sin (inxi surfAngle) + (inxi surfLoad) * (sin (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (inxi baseHydroForce)),
  
  foldlSP [S "Using", ch nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  plural value, S "as done in", eqN 3],
  
  eqUnR' $
  inxi shearRNoIntsl $= (inxi nrmFNoIntsl) * tan (inxi fricAngle) +
  (inxi cohesion) * (inxi baseWthX) * sec (inxi baseAngle) $=
  (((inxi slcWght) + (inxi surfHydroForce) * (cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (cos (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi watrForceDif) +
  (inxi surfHydroForce) * sin (inxi surfAngle) + (inxi surfLoad) *
  (sin (inxi impLoadAngle))) * (sin (inxi baseAngle)) -
  (inxi baseHydroForce)) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)

  ]

------------------------------------------------------------------

mobShr_deriv_sentences_ssp_s1 :: [Sentence]
mobShr_deriv_sentences_ssp_s1 = [S "The", phrase mobShrI, S "acting on a slice is defined as",
  ch mobShrI, S "from the force equilibrium in", makeRef genDef2Label `sC`
  S "also shown in", eqN 4]

mobShr_deriv_sentences_ssp_s2 :: [Sentence]
mobShr_deriv_sentences_ssp_s2 = [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce,
  S "Consider a force equilibrium", S wiif `sC` S "to obtain the",
  getTandS shearFNoIntsl `sC` S "as done in", eqN 5]

mobShr_deriv_sentences_ssp_s3 :: [Sentence]
mobShr_deriv_sentences_ssp_s3 = [S "The" +:+ plural value +:+ S "of" +:+ ch shearRNoIntsl `sAnd`
  ch shearFNoIntsl +:+ S "are now defined completely in terms of the" +:+
  S "known force property" +:+ plural value +:+ S "of" +:+ ddRef sliceWght +:+ S "to" +:+. ddRef lengthLs]


mobShrDerivation_sentence :: [Sentence]
mobShrDerivation_sentence = map foldlSentCol [mobShr_deriv_sentences_ssp_s1, mobShr_deriv_sentences_ssp_s2]

mobShr_deriv_eqns_ssp :: [Expr]
mobShr_deriv_eqns_ssp = [eq4, eq5]

eq4, eq5:: Expr
eq4 = inxi mobShrI $= eqlExpr sin cos
    (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

eq5 = inxi shearFNoIntsl $= ((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (cos (inxi baseAngle))

  ------old chunk-----
mobShrDerivation :: [Contents]
mobShrDerivation = [

  foldlSP [S "The", phrase mobShrI, S "acting on a slice is defined as",
  ch mobShrI, S "from the force equilibrium in", makeRef genDef2Label `sC`
  S "also shown in", eqN 4],
  
  eqUnR' $ inxi mobShrI $= eqlExpr sin cos
    (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y),
  
  foldlSP [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce,
  S "Consider a force equilibrium", S wiif `sC` S "to obtain the",
  getTandS shearFNoIntsl `sC` S "as done in", eqN 5],
  
  eqUnR' $
  inxi shearFNoIntsl $= ((inxi slcWght) + (inxi surfHydroForce) *
  (cos (inxi surfAngle)) + (inxi surfLoad) * (cos (inxi impLoadAngle))) *
  (sin (inxi baseAngle)) - (negate (sy earthqkLoadFctr) * (inxi slcWght) -
  (inxi watrForceDif) + (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (cos (inxi baseAngle)),
  
  foldlSP [S "The", plural value, S "of", ch shearRNoIntsl `sAnd`
  ch shearFNoIntsl, S "are now defined completely in terms of the",
  S "known force property", plural value, S "of", ddRef sliceWght, S "to", 
  ddRef lengthLs]

  ]

kiStar :: Expr
kiStar = m2x2 (inxi shrStiffBase * cos(inxi baseAngle))
  (negate $ inxi nrmStiffBase * sin(inxi baseAngle)) (inxi shrStiffBase *
  sin(inxi baseAngle)) (inxi nrmStiffBase * cos(inxi baseAngle))
  
kiPrime :: Expr
kiPrime = m2x2
  (inxi shrStiffBase * cos(inxi baseAngle) $^ 2 + inxi nrmStiffIntsl *
  sin(inxi baseAngle) $^ 2) ((inxi shrStiffBase - inxi nrmStiffBase) *
  sin(inxi baseAngle) * cos(inxi baseAngle)) ((inxi shrStiffBase -
  inxi nrmStiffBase) * sin(inxi baseAngle) * cos(inxi baseAngle))
  (inxi shrStiffBase * cos(inxi baseAngle) $^ 2 + inxi nrmStiffIntsl *
  sin(inxi baseAngle) $^ 2)


------------------------------------------------------- 

stfMtrx_deriv_sentences_ssp_s1 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s1 = [S "For interslice surfaces the stiffness constants" `sAnd`
  S "displacements refer to an unrotated coordinate system" `sC`
  ch genDisplace +:+ S "of" +:+. ddRef lengthLs +:+ S "The interslice elements" +:+
  S "are left in their standard coordinate system" `sC`
  S "and therefore are described by the same" +:+ phrase equation +:+
  S "from" +:+. makeRef genDef8Label +:+ S "Seen as" +:+ ch shrStiffIntsl +:+ S "in" +:+.
  ddRef intrsliceF +:+ isElemInMx shrStiffIntsl "shear" `sC` --FIXEME: add matrix symbols?
  S "and" +:+ (isElemInMx nrmStiffIntsl "normal" `sC` S "calculated as in") +:+. ddRef mobShearWO]
  
stfMtrx_deriv_sentences_ssp_s2 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s2 =
 [S "For basal surfaces the stiffness constants" `sAnd`
  S "displacements refer to a system rotated for the base angle alpha" +:+.
  sParen (ddRef angleA) +:+ S "To analyze the effect of force-displacement" +:+
  S "relationships occurring on both basal" `sAnd`
  S "interslice surfaces of an" +:+ phrase element +:+ ch index +:+
  S "they must reference the same coordinate" +:+
  S "system. The basal stiffness matrix must be rotated counter clockwise" +:+
  S "to align with" +:+. (phrase angle `ofThe` S "basal surface") +:+
  S "The base stiffness counter clockwise rotation is applied in" +:+ eqN 7 +:+
  S "to the new matrix" +:+. ch nrmFNoIntsl]

stfMtrx_deriv_sentences_ssp_s3 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s3 = [S "The Hooke's law force displacement relationship of" +:+ (makeRef genDef8Label) +:+
  S "applied to the base also references a displacement vector" +:+
  ch rotatedDispl +:+ S "of" +:+ makeRef genDef9Label +:+ S "rotated for the base angle of the slice" +:+ 
  ch baseAngle +:+. S "The basal displacement vector" +:+
  ch genDisplace +:+  S "is rotated clockwise to align with the" +:+
  phrase intrslce +:+ S "displacement vector" +:+
  ch genDisplace `sC` S "applying the" +:+ phrase definition +:+ S "of" +:+ 
  ch rotatedDispl +:+ S "in terms of" +:+ ch genDisplace +:+ S "as seen in" +:+.
  makeRef genDef9Label +:+ S "Using this with base stiffness matrix" +:+
  ch shrStiffBase --FIXME: should be K*i"
  `sC` S "a basal force displacement relationship in the same coordinate" +:+
  S "system as the interslice relationship can be derived as done in" +:+. eqN 8]

stfMtrx_deriv_sentences_ssp_s4 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s4 = 
  [S "The new effective base stiffness matrix" +:+ ch shrStiffBase +:+
  --FIXME: add symbol?
  S "as derived in" +:+ eqN 7 +:+ S "is defined in" +:+. eqN 9 +:+
  isElemInMx shrStiffBase "shear" `sC` S "and" +:+ isElemInMx nrmStiffBase "normal" `sC` 
  S "calculated as in" +:+. ddRef mobShearWO +:+ S "The notation is simplified by" +:+ 
  S "the introduction of the constants" +:+ ch effStiffA `sAnd` ch effStiffB `sC` 
  S "defined in" +:+ eqN 10 `sAnd` eqN 11 +:+. S "respectively"]

stfMtrx_deriv_sentences_ssp_s5 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s5 = [S "A force-displacement relationship for an element" +:+ ch index +:+
  S "can be written in terms of displacements occurring in the unrotated" +:+
  S "coordinate system" +:+ ch genDisplace `sOf` makeRef genDef9Label +:+ S "using the matrix" +:+
  ch shrStiffBase `sC` --FIXME: index 
  S "and" +:+ ch shrStiffBase +:+ S "as seen in" +:+. ddRef intrsliceF]


eq6, eq7, eq8, eq9, eq10, eq11:: Expr
eq6 = inxi shrStiffIntsl $=
  dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase)

eq7 = inxi shrStiffIntsl $=
  m2x2 (cos(inxi baseAngle)) (negate $ sin(inxi baseAngle))
  (sin(inxi baseAngle)) (cos(inxi baseAngle)) *
  inxi shrStiffIntsl $= kiStar

eq8 = vec2D (inxi genPressure) (inxi genPressure) $=
  inxi shrStiffBase * sy rotatedDispl $= --FIXME: add more symbols?
  kiStar * rotMtx * displMtx $= kiPrime * displMtx

eq9 = inxi shrStiffBase $= kiPrime
  $= m2x2 (inxi effStiffA) (inxi effStiffB) (inxi effStiffB) (inxi effStiffA)

eq10 = (inxi effStiffA) $= (inxi shrStiffBase) * (cos (inxi baseAngle)) $^ 2 +
  (inxi nrmStiffBase) * (sin (inxi baseAngle)) $^ 2

eq11 = (inxi effStiffB) $= ((inxi shrStiffBase)-(inxi nrmStiffBase)) *
  (sin (inxi baseAngle)) * (cos (inxi baseAngle))
--------------old chunk------------
stfMtrxDerivation :: [Contents]
stfMtrxDerivation = [

  foldlSP [S "Using the force-displacement relationship of", 
  makeRef genDef8Label, S "to define stiffness matrix", ch shrStiffIntsl `sC`
  S "as seen in", eqN 6],
  
  eqUnR' $ inxi shrStiffIntsl $=
  dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase),
  
  foldlSP [S "For interslice surfaces the stiffness constants" `sAnd`
  S "displacements refer to an unrotated coordinate system" `sC`
  ch genDisplace, S "of" +:+. ddRef lengthLs, S "The interslice elements",
  S "are left in their standard coordinate system" `sC`
  S "and therefore are described by the same", phrase equation,
  S "from" +:+. makeRef genDef8Label, S "Seen as", ch shrStiffIntsl, S "in" +:+.
  ddRef intrsliceF, isElemInMx shrStiffIntsl "shear" `sC` --FIXEME: add matrix symbols?
  S "and", isElemInMx nrmStiffIntsl "normal" `sC` S "calculated as in", ddRef mobShearWO],
  
  foldlSP [S "For basal surfaces the stiffness constants" `sAnd`
  S "displacements refer to a system rotated for the base angle alpha" +:+.
  sParen (ddRef angleA), S "To analyze the effect of force-displacement",
  S "relationships occurring on both basal" `sAnd`
  S "interslice surfaces of an", phrase element, ch index,
  S "they must reference the same coordinate",
  S "system. The basal stiffness matrix must be rotated counter clockwise",
  S "to align with" +:+. (phrase angle `ofThe` S "basal surface"),
  S "The base stiffness counter clockwise rotation is applied in", eqN 7,
  S "to the new matrix", ch nrmFNoIntsl],
  
  eqUnR' $ inxi shrStiffIntsl $=
  m2x2 (cos(inxi baseAngle)) (negate $ sin(inxi baseAngle))
  (sin(inxi baseAngle)) (cos(inxi baseAngle)) *
  inxi shrStiffIntsl $= kiStar,
  
  foldlSP [S "The Hooke's law force displacement relationship of", makeRef genDef8Label,
  S "applied to the base also references a displacement vector",
  ch rotatedDispl, S "of", makeRef genDef9Label, S "rotated for", S "base angle" `ofThe`
  S "slice", ch baseAngle +:+. S "The basal displacement vector",
  ch genDisplace,  S "is rotated clockwise to align with the",
  phrase intrslce, S "displacement vector",
  ch genDisplace `sC` S "applying the", phrase definition, S "of", 
  ch rotatedDispl, S "in terms of", ch genDisplace, S "as seen in" +:+.
  makeRef genDef9Label, S "Using this with base stiffness matrix",
  ch shrStiffBase --FIXME: should be K*i"
  `sC` S "a basal force displacement relationship in the same coordinate",
  S "system as the interslice relationship can be derived as done in", eqN 8],
  
  eqUnR' $ vec2D (inxi genPressure) (inxi genPressure) $=
  inxi shrStiffBase * sy rotatedDispl $= --FIXME: add more symbols?
  kiStar * rotMtx * displMtx $= kiPrime * displMtx,
  
  foldlSP [S "The new effective base stiffness matrix", ch shrStiffBase,
  --FIXME: add symbol?
  S "as derived in", eqN 7, S "is defined in" +:+. eqN 9,
  S "This is seen as matrix", ch shrStiffBase, S "in" +:+.
  S "GD12", isElemInMx shrStiffBase "shear" `sC` S "and", -- FIXME: GD12 doesn't exist
  isElemInMx nrmStiffBase "normal" `sC` S "calculated as in" +:+. ddRef mobShearWO,
  S "The notation is simplified by", S "introduction" `ofThe` S "constants",
  ch effStiffA `sAnd` ch effStiffB `sC` S "defined in", eqN 10 `sAnd`
  eqN 11, S "respectively"],
  
  eqUnR' $ inxi shrStiffBase $= kiPrime
  $= m2x2 (inxi effStiffA) (inxi effStiffB) (inxi effStiffB) (inxi effStiffA),
  
  eqUnR' $
  (inxi effStiffA) $= (inxi shrStiffBase) * (cos (inxi baseAngle)) $^ 2 +
  (inxi nrmStiffBase) * (sin (inxi baseAngle)) $^ 2,
  
  eqUnR' $
  (inxi effStiffB) $= ((inxi shrStiffBase)-(inxi nrmStiffBase)) *
  (sin (inxi baseAngle)) * (cos (inxi baseAngle)),
  
  foldlSP [S "A force-displacement relationship for an element", ch index,
  S "can be written in terms of displacements occurring in the unrotated", 
  S "coordinate system", ch genDisplace `sOf` makeRef genDef9Label, S "using the matrix",
  ch shrStiffBase `sC` --FIXME: index 
  S "and", ch shrStiffBase, S "as seen in", ddRef intrsliceF]
  
  ]

isElemInMx :: (Quantity a) => a -> String -> Sentence
isElemInMx sym kword = ch sym `isThe` S kword +:+ S "element in the matrix"
