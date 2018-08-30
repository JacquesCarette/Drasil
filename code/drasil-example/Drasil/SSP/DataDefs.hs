module Drasil.SSP.DataDefs (dataDefns, soilStiffness, netFDsplcmntEqbm, 
  mobShearWO, displcmntRxnF, intrsliceF, surfLoads, seismicLoadF, 
  resShearWO, lengthLs, lengthLb, sliceWght, fixme1, fixme2,
  stfMtrxDerivation, mobShrDerivation, resShrDerivation) where 

import Prelude hiding (cos, sin, tan)
import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Quantities.SolidMechanics as SM (poissnsR)
import Data.Drasil.Utils (eqUnR', weave)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (definition, element, value)
import Data.Drasil.SentenceStructures (eqN, foldlSentCol, foldlSP, getTandS, 
  isThe, ofThe, ofThe', sAnd, sOf)
import Data.Drasil.Concepts.Math (angle, equation)


import Drasil.SSP.BasicExprs (displMtx, eqlExpr, rotMtx)
import Drasil.SSP.Defs (intrslce)
import Drasil.SSP.Labels (genDef2Label, genDef3Label, genDef8Label, genDef9Label,
  sliceWghtL, baseWtrFL, surfWtrFL, intersliceWtrFL, angleAL, angleBL, lengthLbL
  , sliceWghtL, lengthBL, lengthLsL, seismicLoadFL, intrsliceFL, resShearWOL
  , mobShearWOL, netFDsplcmntEqbmL, shearStiffnessL, soilStiffnessL, displcmntBaselL
  , displcmntRxnFL, surfLoadsL)
import Drasil.SSP.TMods (effStress)
import Drasil.SSP.References (chen2005, fredlund1977, stolle2008)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, constant_A, constant_K, constant_a, dryWeight, earthqkLoadFctr, 
  effStiffA, effStiffB, fricAngle, genDisplace, genForce, genPressure, 
  impLoadAngle, index, intNormForce, intShrForce, inx, inxi, inxiM1, mobShrI, 
  normStress, normToShear, nrmDispl, nrmFNoIntsl, nrmFSubWat, nrmStiffBase, 
  nrmStiffIntsl, poissnsRatio, rotatedDispl, satWeight, scalFunc, shearFNoIntsl, 
  shearRNoIntsl, shrDispl, shrResI, shrStiffBase, shrStiffIntsl, slcWght, 
  slipDist, slipHght, slopeDist, slopeHght, surfAngle, surfHydroForce, surfLngth, 
  surfLoad, ufixme1, ufixme2, waterHght, waterWeight, watrForce, watrForceDif, wiif)

------------------------
--  Data Definitions  --
------------------------

dataDefns :: [DataDefinition]
dataDefns = [sliceWght, baseWtrF, surfWtrF, intersliceWtrF, angleA, angleB, 
  lengthB, lengthLb, lengthLs, seismicLoadF, surfLoads, intrsliceF, resShearWO,
  mobShearWO, displcmntRxnF, displcmntBasel, netFDsplcmntEqbm, shearStiffness,
  soilStiffness, fixme1, fixme2]

--DD1

sliceWght :: DataDefinition
sliceWght = mkDDL sliceWghtQD [makeRef fredlund1977] [{-Derivation-}] sliceWghtL Nothing--Notes
--FIXME: fill empty lists in

sliceWghtQD :: QDefinition
sliceWghtQD = mkQuantDef slcWght slcWgtEqn

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

baseWtrF :: DataDefinition
baseWtrF = mkDDL baseWtrFQD [makeRef fredlund1977] [{-Derivation-}] baseWtrFL Nothing--Notes
--FIXME: fill empty lists in

baseWtrFQD :: QDefinition
baseWtrFQD = mkQuantDef baseHydroForce bsWtrFEqn 

bsWtrFEqn :: Expr
bsWtrFEqn = (inxi baseLngth)*(case_ [case1,case2])
  where case1 = (((inxi waterHght)-(inxi slipHght))*(sy waterWeight),
          (inxi waterHght) $> (inxi slipHght))

        case2 = (0, (inxi waterHght) $<= (inxi slipHght))

--DD3

surfWtrF :: DataDefinition
surfWtrF = mkDDL surfWtrFQD [makeRef fredlund1977] [{-Derivation-}] surfWtrFL Nothing--Notes
--FIXME: fill empty lists in

surfWtrFQD :: QDefinition
surfWtrFQD = mkQuantDef surfHydroForce surfWtrFEqn

surfWtrFEqn :: Expr
surfWtrFEqn = (inxi surfLngth)*(case_ [case1,case2])
  where case1 = (((inxi waterHght)-(inxi slopeHght))*(sy waterWeight),
          (inxi waterHght) $> (inxi slopeHght))

        case2 = (0, (inxi waterHght) $<= (inxi slopeHght))

--DD4

intersliceWtrF :: DataDefinition
intersliceWtrF = mkDDL intersliceWtrFQD [makeRef fredlund1977] [{-Derivation-}] intersliceWtrFL Nothing--Notes
--FIXME: fill empty lists in

intersliceWtrFQD :: QDefinition
intersliceWtrFQD = mkQuantDef watrForce intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = case_ [case1,case2,case3]
  where case1 = (((inxi slopeHght)-(inxi slipHght ))$^ 2 / 2  *
          (sy satWeight) + ((inxi waterHght)-(inxi slopeHght))$^ 2 *
          (sy satWeight), (inxi waterHght) $>= (inxi slopeHght))

        case2 = (((inxi waterHght)-(inxi slipHght ))$^ 2 / 2  * (sy satWeight),
                (inxi slopeHght) $> (inxi waterHght) $> (inxi slipHght))

        case3 = (0,(inxi waterHght) $<= (inxi slipHght))

--DD5

angleA :: DataDefinition
angleA = mkDDL angleAQD [makeRef fredlund1977] [{-Derivation-}] angleAL Nothing--Notes
--FIXME: fill empty lists in

angleAQD :: QDefinition
angleAQD = mkQuantDef baseAngle angleAEqn

angleAEqn :: Expr
angleAEqn = (inxi slipHght - inx slipHght (-1)) /
  (inxi slipDist - inx slipDist (-1))

--DD5.5

angleB :: DataDefinition
angleB = mkDDL angleBQD [makeRef fredlund1977] [{-Derivation-}] angleBL Nothing--Notes
--FIXME: fill empty lists in

angleBQD :: QDefinition
angleBQD = mkQuantDef surfAngle angleBEqn

angleBEqn :: Expr
angleBEqn = (inxi slopeHght - inx slopeHght (-1)) /
  (inxi slopeDist - inx slopeDist (-1))

--DD6

lengthB :: DataDefinition
lengthB = mkDDL lengthBQD [makeRef fredlund1977] [{-Derivation-}] lengthBL Nothing--Notes
--FIXME: fill empty lists in

lengthBQD :: QDefinition
lengthBQD = mkQuantDef baseWthX lengthBEqn

lengthBEqn :: Expr
lengthBEqn = inxi slipDist - inx slipDist (-1)

--DD6.3

lengthLb :: DataDefinition
lengthLb = mkDDL lengthLbQD [makeRef fredlund1977] [{-Derivation-}] lengthLbL Nothing--Notes
--FIXME: fill empty lists in

lengthLbQD :: QDefinition
lengthLbQD = mkQuantDef baseLngth lengthLbEqn

lengthLbEqn :: Expr
lengthLbEqn = (inxi baseWthX) * sec (inxi baseAngle)

--DD6.6

lengthLs :: DataDefinition
lengthLs = mkDDL lengthLsQD [makeRef fredlund1977] [{-Derivation-}] lengthLsL Nothing--Notes
--FIXME: fill empty lists in

lengthLsQD :: QDefinition
lengthLsQD = mkQuantDef surfLngth lengthLsEqn

lengthLsEqn :: Expr
lengthLsEqn = (inxi baseWthX) * sec (inxi surfAngle)

--DD7

seismicLoadF :: DataDefinition
seismicLoadF = mkDDL seismicLoadFQD [makeRef fredlund1977] [{-Derivation-}] seismicLoadFL Nothing--Notes
--FIXME: fill empty lists in

seismicLoadFQD :: QDefinition
seismicLoadFQD = mkQuantDef earthqkLoadFctr ssmcLFEqn
  --FIXME: K_E missing for unitals?

ssmcLFEqn :: Expr
ssmcLFEqn = ((sy earthqkLoadFctr) * (inxi slcWght))

--DD8

surfLoads :: DataDefinition
surfLoads = mkDDL surfLoadsQD [makeRef chen2005] [{-Derivation-}] surfLoadsL Nothing--Notes
--FIXME: fill empty lists in

surfLoadsQD :: QDefinition
surfLoadsQD = mkQuantDef surfLoad surfLEqn
  --FIXEME: is this data definition necessary?

surfLEqn :: Expr
surfLEqn = (inxi surfLoad) * (inxi impLoadAngle)
  --FIXME: should be split into two DataDefs

--DD9

intrsliceF :: DataDefinition
intrsliceF = mkDDL intrsliceFQD [makeRef chen2005] [{-Derivation-}] intrsliceFL Nothing--Notes
--FIXME: fill empty lists in

intrsliceFQD :: QDefinition
intrsliceFQD = mkQuantDef intShrForce intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (sy normToShear) * (inxi scalFunc) * (inxi intNormForce)

--DD10

resShearWO :: DataDefinition
resShearWO = mkDDL resShearWOQD [makeRef chen2005] resShr_deriv_ssp resShearWOL Nothing--Notes
--FIXME: fill empty lists in

resShearWOQD :: QDefinition
resShearWOQD = mkQuantDef' shearRNoIntsl resShearWOEqn

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

mobShearWO :: DataDefinition
mobShearWO = mkDDL mobShearWOQD [makeRef chen2005] mobShr_deriv_ssp mobShearWOL Nothing--Notes
--FIXME: fill empty lists in

mobShearWOQD :: QDefinition
mobShearWOQD = mkQuantDef' shearFNoIntsl mobShearWOEqn

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

displcmntRxnF :: DataDefinition
displcmntRxnF = mkDDL displcmntRxnFQD [makeRef stolle2008] [{-Derivation-}] displcmntRxnFL Nothing--Notes
--FIXME: fill empty lists in

displcmntRxnFQD :: QDefinition
displcmntRxnFQD = mkQuantDef genPressure displcmntRxnFEqn 

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase) * displMtx

--DD12.5

displcmntBasel :: DataDefinition
displcmntBasel = mkDDL displcmntBaselQD [makeRef stolle2008] stfMtrx_deriv_ssp displcmntBaselL Nothing--Notes
--FIXME: fill empty lists in

displcmntBaselQD :: QDefinition
displcmntBaselQD = mkQuantDef' genPressure displcmntBaselEqn

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

netFDsplcmntEqbm :: DataDefinition
netFDsplcmntEqbm = mkDDL netFDsplcmntEqbmQD [makeRef stolle2008] [{-Derivation-}] netFDsplcmntEqbmL Nothing--Notes
--FIXME: fill empty lists in

netFDsplcmntEqbmQD :: QDefinition
netFDsplcmntEqbmQD = mkQuantDef genForce netFDsplcmntEqbmEqn 

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = negate (inx surfLngth (-1)) * (inx nrmStiffIntsl (-1)) *
  (inx genDisplace (-1)) + (inx surfLngth (-1) * inx nrmStiffIntsl (-1) +
  inx baseLngth 0 * inx nrmStiffBase 0 + inx surfLngth 0 *
  inx nrmStiffIntsl 0) * (inx genDisplace 0) -
  (inx surfLngth 0) * (inx nrmStiffIntsl 0) * (inx genDisplace 1)

--DD14

shearStiffness :: DataDefinition
shearStiffness = mkDDL shearStiffnessQD [makeRef stolle2008] [{-Derivation-}] shearStiffnessL Nothing--Notes
--FIXME: fill empty lists in

shearStiffnessQD :: QDefinition
shearStiffnessQD = mkQuantDef shrStiffBase shearStiffnessEqn  

shearStiffnessEqn :: Expr
shearStiffnessEqn = sy intNormForce / (2 * (1 + sy poissnsRatio)) *
  (dbl 0.1 / sy baseWthX) + (inxi cohesion - sy normStress *
  tan(inxi fricAngle)) / (abs (sy shrDispl) + sy constant_a)

--DD15 this is the second part to the original DD14

soilStiffness :: DataDefinition
soilStiffness = mkDDL soilStiffnessQD [makeRef stolle2008] [{-Derivation-}] soilStiffnessL Nothing--Notes
--FIXME: fill empty lists in

soilStiffnessQD :: QDefinition
soilStiffnessQD = mkQuantDef nrmStiffBase soilStiffnessEqn

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

fixme1, fixme2 :: DataDefinition
fixme1 = mkDD fixme1QD [{-References-}] [{-Derivation-}] "fixme1" Nothing--Notes
fixme2 = mkDD fixme2QD [{-References-}] [{-Derivation-}] "fixme2" Nothing--Notes
--FIXME: fill empty lists in

fixme1QD :: QDefinition
fixme1QD = ec ufixme1 (inxi intNormForce + inxiM1 intNormForce)

fixme2QD :: QDefinition
fixme2QD = ec ufixme2 (inxi watrForce + inxiM1 watrForce)

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
  S "of", makeRef effStress, S "shown in", eqN 1]

resShr_deriv_sentences_ssp_s2 :: [Sentence]
resShr_deriv_sentences_ssp_s2 = [plural value `ofThe'` S "interslice forces",
  ch intNormForce `sAnd` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  makeRef sliceWght, S "to" +:+. makeRef lengthLs,
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
  S "of", makeRef effStress, S "shown in", eqN 1],
  
  eqUnR' $ (inxi nrmFSubWat) $= eqlExpr cos sin (\x y -> x -
  inxiM1 intShrForce + inxi intShrForce + y) - inxi baseHydroForce,
  
  foldlSP [plural value `ofThe'` S "interslice forces",
  ch intNormForce `sAnd` ch intShrForce, S "in the", phrase equation,
  S "are unknown, while the other", plural value,
  S "are found from the physical force", plural definition, S "of",
  makeRef sliceWght, S "to" +:+. makeRef lengthLs,
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
  S "known force property" +:+ plural value +:+ S "of" +:+ makeRef sliceWght +:+ S "to" +:+. makeRef lengthLs]


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
  S "known force property", plural value, S "of", makeRef sliceWght, S "to", 
  makeRef lengthLs]

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
  ch genDisplace +:+ S "of" +:+. makeRef lengthLs +:+ S "The interslice elements" +:+
  S "are left in their standard coordinate system" `sC`
  S "and therefore are described by the same" +:+ phrase equation +:+
  S "from" +:+. makeRef genDef8Label +:+ S "Seen as" +:+ ch shrStiffIntsl +:+ S "in" +:+.
  makeRef intrsliceF +:+ isElemInMx shrStiffIntsl "shear" `sC` --FIXEME: add matrix symbols?
  S "and" +:+ (isElemInMx nrmStiffIntsl "normal" `sC` S "calculated as in") +:+. makeRef mobShearWO]
  
stfMtrx_deriv_sentences_ssp_s2 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s2 =
 [S "For basal surfaces the stiffness constants" `sAnd`
  S "displacements refer to a system rotated for the base angle alpha" +:+.
  sParen (makeRef angleA) +:+ S "To analyze the effect of force-displacement" +:+
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
  S "calculated as in" +:+. makeRef mobShearWO +:+ S "The notation is simplified by" +:+ 
  S "the introduction of the constants" +:+ ch effStiffA `sAnd` ch effStiffB `sC` 
  S "defined in" +:+ eqN 10 `sAnd` eqN 11 +:+. S "respectively"]

stfMtrx_deriv_sentences_ssp_s5 :: [Sentence]
stfMtrx_deriv_sentences_ssp_s5 = [S "A force-displacement relationship for an element" +:+ ch index +:+
  S "can be written in terms of displacements occurring in the unrotated" +:+
  S "coordinate system" +:+ ch genDisplace `sOf` makeRef genDef9Label +:+ S "using the matrix" +:+
  ch shrStiffBase `sC` --FIXME: index 
  S "and" +:+ ch shrStiffBase +:+ S "as seen in" +:+. makeRef intrsliceF]


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
  ch genDisplace, S "of" +:+. makeRef lengthLs, S "The interslice elements",
  S "are left in their standard coordinate system" `sC`
  S "and therefore are described by the same", phrase equation,
  S "from" +:+. makeRef genDef8Label, S "Seen as", ch shrStiffIntsl, S "in" +:+.
  makeRef intrsliceF, isElemInMx shrStiffIntsl "shear" `sC` --FIXEME: add matrix symbols?
  S "and", isElemInMx nrmStiffIntsl "normal" `sC` S "calculated as in", makeRef mobShearWO],
  
  foldlSP [S "For basal surfaces the stiffness constants" `sAnd`
  S "displacements refer to a system rotated for the base angle alpha" +:+.
  sParen (makeRef angleA), S "To analyze the effect of force-displacement",
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
  isElemInMx nrmStiffBase "normal" `sC` S "calculated as in" +:+. makeRef mobShearWO,
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
  S "and", ch shrStiffBase, S "as seen in", makeRef intrsliceF]
  
  ]

isElemInMx :: (Quantity a) => a -> String -> Sentence
isElemInMx sym kword = ch sym `isThe` S kword +:+ S "element in the matrix"
