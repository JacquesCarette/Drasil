module Drasil.SSP.IMods where
--(sspIMods), currently weaves the derivations in body.hs

import Prelude hiding (tan, product, sin, cos)
import Language.Drasil
import Control.Lens ((^.))

import Drasil.SSP.Unitals (inxi, shrStress, baseLngth, sum1toN, mobStress,
  fs, fs_min, fsloc, shrDispl, shrStiffBase, genForce, constant_a, fricAngle,
  normStress, baseWthX, cohesion, poissnsRatio, intNormForce, nrmStiffBase,
  nrmDispl, dy_i, dx_i, baseAngle, genDisplace, rotatedDispl, index, yi,
  xi, numbSlices, shrResC, shearRNoIntsl, shearFNoIntsl, mobShrC,
  inxi, inxiP1, normToShear, scalFunc, intShrForce, wiif, inxiM1, totNrmForce,
  nrmFSubWat, mobShrI, baseHydroForce, impLoadAngle, surfLoad, surfAngle,
  surfHydroForce, earthqkLoadFctr, slcWght, midpntHght, watrForce, critCoords,
  indxn, minFunction, surfLngth, shrStiffIntsl, watrForceDif, effStiffB,
  effStiffA, nrmStiffIntsl, indx1, normFunc, shearFunc, varblU, varblV)
import Drasil.SSP.Defs (slope, slice, slip,
  intrslce, ssa, morPrice, crtSlpSrf, factorOfSafety)
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Data.Drasil.Utils (getES, eqUnR, weave)
import Drasil.SSP.DataDefs (fixme1,fixme2, ddRef, sliceWght, baseWtrF, surfWtrF, intersliceWtrF, angleA, angleB,
  lengthB, lengthLb, lengthLs, seismicLoadF, surfLoads, intrsliceF, resShearWO,
  mobShearWO, displcmntRxnF, displcmntBasel, netFDsplcmntEqbm, shearStiffness,
  soilStiffness)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, intrslce, morPrice, slice, 
  slip, slope, ssa)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  cohesion, constant_a, critCoords, dx_i, dy_i, earthqkLoadFctr, effStiffA, 
  effStiffB, fricAngle, fs, fs_min, fsloc, genDisplace, genForce, impLoadAngle, 
  index, indx1, indxn, intNormForce, intShrForce, inxi, inxi, inxiM1, inxiP1, 
  midpntHght, minFunction, mobShrC, mobShrI, mobStress, normFunc, normStress, 
  normToShear, nrmDispl, nrmFSubWat, nrmStiffBase, nrmStiffIntsl, numbSlices, 
  poissnsRatio, rotatedDispl, scalFunc, shearFNoIntsl, shearFunc, shearRNoIntsl, 
  shrDispl, shrResC, shrStiffBase, shrStiffIntsl, shrStress, slcWght, sum1toN, 
  surfAngle, surfHydroForce, surfLngth, surfLoad, totNrmForce, varblU, varblV,
  watrForce, watrForceDif, wiif, xi, yi)
import Drasil.SSP.Assumptions (newA2, sspRefDB)
import Drasil.DocumentLanguage.RefHelpers(refA)

import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Data.Drasil.Utils (eqUnR, getES)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis,
  solution, definition, value, assumption, physicalProperty,
  problem, method_)
import Data.Drasil.SentenceStructures (andThe, acroA, acroGD,
  sIs, sIn, getTDS, getTandS, ofThe, ofThe', sAnd, sOf, acroIM, acroT,
  eqN, foldlSP, foldlSent_,foldlSentCol)
import Data.Drasil.Concepts.Math (equation, surface)
import Data.Drasil.Concepts.Physics (displacement, force)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Drasil.SSP.GenDefs
import Drasil.SSP.BasicExprs (displMtx, eqlExpr, momExpr, rotMtx)
-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [RelationConcept]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb, rfemFoS, crtSlpId]

sspIMods_new :: [InstanceModel]
sspIMods_new = [fctSfty_new, nrmShrFor_new, intsliceFs_new, forDisEqlb_new,
 rfemFoS_new, crtSlpId_new]

--
fctSfty_new :: InstanceModel
fctSfty_new = im'' fctSfty [qw shearRNoIntsl, qw shearFNoIntsl,
 qw mobShrC, qw shrResC, qw varblV]
  [] (qw fs) [] fctSfty_deriv_ssp "fctSfty" [fcSfty_desc]

fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel 

--FIXME: first shearRNoIntsl should have local index v, not i,
--       last occurence should have index n
--       similar case with shearFNoIntsl
fcSfty_rel :: Relation
fcSfty_rel = sy fs $= sumOp shearRNoIntsl / sumOp shearFNoIntsl
  where prodOp = defprod lU (sy index) (sy numbSlices - 1)
          (idx (sy mobShrC) (sy varblU) / idx (sy shrResC) (sy varblU))
        sumOp sym = defsum lV 1 (sy numbSlices - 1)
          (idx (sy sym) (sy varblV) * prodOp) + idx (sy sym) (sy numbSlices)

fcSfty_desc :: Sentence
fcSfty_desc = foldlSent [S "Equation for the", titleize fs `isThe` S "ratio",
  S "between resistive and mobile shear of the slip surface.",
  S "The sum of values from each slice is taken to find the total",
  S "resistive and mobile shear for the slip surface. The constants",
  getES shrResC, S "and", getES mobShrC, S "convert the resistive and",
  S "mobile shear without the inluence of",
  --FIXME: have these constants defined somewhere else
  S "interslice forces, to a calculation considering the interslice forces"]

--
nrmShrFor_new :: InstanceModel
nrmShrFor_new = im'' nrmShrFor [qw baseWthX, qw scalFunc,
 qw watrForce, qw baseAngle, qw midpntHght, 
 qw earthqkLoadFctr, qw slcWght, qw surfHydroForce]
  [TCon AssumedCon $ sy fixme1 $< sy fixme1] (qw shearFunc)
   [TCon AssumedCon $ 0 $< sy fixme1 $< sy fixme1] nrmShr_deriv_ssp "nrmShrFor" [nrmShrF_desc]

nrmShrFor :: RelationConcept
nrmShrFor = makeRC "nrmShrFor" (nounPhraseSP "normal/shear force ratio")
  nrmShrF_desc nrmShrF_rel

nrmShrF_rel :: Relation
nrmShrF_rel = (sy normFunc) $= case_ [case1,case2,case3] $=
  sy shearFunc $= case_ [
  (indx1 baseWthX * indx1 scalFunc * indx1 intNormForce, sy index $= 1),
  (inxi baseWthX * (inxi scalFunc * inxi intNormForce +
    inxiM1 scalFunc  * inxiM1 intNormForce),
    2 $<= sy index $<= (sy numbSlices - 1)),
  (indxn baseWthX * idx (sy intNormForce) (sy numbSlices - 1) *
    idx (sy watrForce) (sy numbSlices - 1), sy index $= 1)
  ]
  $= --FIXME: move to seperate instance model
  sy normToShear $= sum1toN (sy normFunc) / sum1toN (sy shearFunc)
  where case1 = ((indx1 baseWthX)*((indx1 intNormForce)+(indx1 watrForce)) *
          tan (indx1 baseAngle), sy index $= 1)
        case2 = ((inxi baseWthX)*
          (sy fixme1 + sy fixme2)
           * tan (inxi baseAngle)+ (sy midpntHght) * (sy earthqkLoadFctr * inxi slcWght -
          2 * inxi surfHydroForce * sin (inxi surfAngle) -
          2 * inxi surfLoad * cos (inxi impLoadAngle)),
          2 $<= sy index $<= ((sy numbSlices) - 1))
        case3 = ((indxn baseWthX)*(idx (sy intNormForce)
          (sy numbSlices - 1) + idx (sy watrForce)
          (sy numbSlices - 1)) * tan (idx (sy baseAngle)
          (sy numbSlices - 1)), sy index $= (sy numbSlices))

nrmShrF_desc :: Sentence
nrmShrF_desc = foldlSent [getES normToShear `isThe` S "magnitude ratio",
  S "between shear and normal forces at the interslice interfaces as the", 
  S "assumption of the Morgenstern Price method in", acroGD 5,
  S "The inclination function", getES scalFunc,
  S "determines the relative magnitude ratio between the",
  S "different interslices, while", getES normToShear, S "determines the" +:+.
  S "magnitude", getES normToShear, S "uses the sum of interslice normal",
  S "and shear forces taken from each interslice"]

--

intsliceFs_new :: InstanceModel
intsliceFs_new = im'' intsliceFs [qw index, qw fs,
  qw shearRNoIntsl, qw shearFNoIntsl,
 qw mobShrC, qw shrResC]
  [] (qw intNormForce) [] intrSlc_deriv_ssp "intsliceFs" [sliceFs_desc]

intsliceFs :: RelationConcept
intsliceFs = makeRC "intsliceFs" (nounPhraseSP "interslice forces")
  sliceFs_desc sliceFs_rel 

sliceFs_rel :: Relation
sliceFs_rel = inxi intNormForce $= case_ [
  (((sy fs) * indx1 shearFNoIntsl - indx1 shearRNoIntsl) / indx1 shrResC,
    sy index $= 1),
  ((inxiM1 mobShrC * inxiM1 intNormForce +
    sy fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC,
    2 $<= sy index $<= ((sy numbSlices) - 1)),
  (0, sy index $= 0 $|| sy index $= sy numbSlices)]  
  -- FIXME: Use index i as part of condition

sliceFs_desc :: Sentence
sliceFs_desc = foldlSent [S "The value of the interslice normal force",
  getES intNormForce, S "at interface", getES index +:+. S "The net force"
  `isThe` S "weight",
  S "of the slices adjacent to interface", getES index,
  S "exert horizontally on each other"]

--
forDisEqlb_new :: InstanceModel
forDisEqlb_new = im'' forDisEqlb [qw baseAngle,
 qw baseHydroForce, qw surfHydroForce, qw surfAngle, qw surfLoad, qw impLoadAngle,
 qw surfLngth, qw nrmStiffIntsl, qw dx_i, qw effStiffA, qw dy_i, qw baseLngth, qw effStiffB]
  [] (qw earthqkLoadFctr) [] rigDis_deriv_ssp "forDisEqlb" [fDisEq_desc']

forDisEqlb :: RelationConcept
forDisEqlb = makeRC "forDisEqlb"
  (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel 

fDisEq_rel :: Relation --FIXME: split into two IMOD
fDisEq_rel = negate (inxi watrForceDif) - (sy earthqkLoadFctr)*(inxi slcWght) -
  (inxi baseHydroForce)*(sin(inxi baseAngle)) +
  (inxi surfHydroForce)*sin(inxi surfAngle) + (inxi surfLoad) *
  sin(inxi impLoadAngle) $= inxiM1  dx_i * (negate (inxiM1 surfLngth) *
  inxiM1 nrmStiffIntsl) + inxi dx_i * (negate (inxiM1 surfLngth) *
  inxiM1 nrmStiffIntsl +
  inxi surfLngth * inxi nrmStiffIntsl + inxi baseLngth * inxi effStiffA) +
  inxiP1  dx_i * (negate (inxi surfLngth) * inxi nrmStiffIntsl) +
  inxi dy_i * (negate (inxi baseLngth) * inxi effStiffB) $=
  negate (inxi slcWght) - (inxi baseHydroForce)*(cos(inxi baseAngle)) +
  (inxi surfHydroForce)*cos(inxi surfAngle) + (inxi surfLoad) *
  cos(inxi impLoadAngle) $= inxiM1  dy_i * (negate (inxiM1 surfLngth) *
  inxiM1 shrStiffIntsl) + inxi dy_i * (negate (inxiM1 surfLngth) *
  inxiM1 shrStiffIntsl + inxi surfLngth * inxi nrmStiffIntsl +
  inxi baseLngth * inxi effStiffA) + inxiP1 dy_i * (negate (inxi surfLngth) *
  inxi shrStiffIntsl) + inxi dx_i * (negate (inxi baseLngth) * inxi effStiffB)

fDisEq_desc' :: Sentence
fDisEq_desc' = foldlSent [
  S "There is one set of force displacement equilibrium equations",
  S "in the x and y directions for each element. System of equations",
  S "solved for displacements (", (getES dx_i), S "and", (getES dy_i), S ")"]

fDisEq_desc :: Sentence
fDisEq_desc = foldlSent [
  S "There is one set of force displacement equilibrium equations",
  S "in the x and y directions for each element. System of equations",
  S "solved for displacements (", (getES dx_i), S "and", (getES dy_i), S ")",
  (getES watrForceDif), S "=",
  (getES watrForce) `isThe` S "net hydrostatic force across a slice.", 
  (getES earthqkLoadFctr) `isThe` S "earthquake load factor.",
  (getES slcWght) `isThe` S "weight of the slice.",
  (getES baseHydroForce)  `isThe` S "pore water pressure acting on the",
  S "slice base.",
  (getES surfHydroForce) `isThe` S "pore water pressure acting on the",
  S "slice surface.",
  (getES baseAngle) `isThe` S "angle of the base with the horizontal.",
  (getES surfAngle) `isThe` S "angle of the surface with the horizontal.",
  (getES dx_i) `isThe` S "x displacement of slice i.",
  (getES dy_i) `isThe` S "y displacement of slice i.",
  (getES surfLngth) `isThe` S "length of the interslice surface i.",
  (getES baseLngth) `isThe` S "length of the base surface i.",
  (getES shrStiffIntsl) `isThe` S "interslice shear stiffness at surface i.",
  S " Kst,i-1" `isThe` S "interslice normal stiffness at surface i.",
  S "KbA,i, and KbB,i", S "are the base stiffness values for slice i"]

--
rfemFoS_new :: InstanceModel
rfemFoS_new = im''' rfemFoS [qw cohesion, qw nrmStiffBase, qw nrmDispl,
 qw fricAngle, qw shrStiffBase, qw shrDispl, qw baseLngth]
  [] (qw fsloc) [] rigFoS_deriv_ssp "rfemFoS"


rfemFoS :: RelationConcept
rfemFoS = makeRC "rfemFoS" (nounPhraseSP "RFEM factor of safety")
  rfemFoS_desc rfemFoS_rel 

rfemFoS_rel :: Relation
rfemFoS_rel = (inxi fsloc) $= fosFracLoc $= fosFracSum

fosFracLoc :: Expr
fosFracLoc = (inxi cohesion - inxi nrmStiffBase * inxi nrmDispl *
  tan(inxi fricAngle)) / (inxi shrStiffBase * inxi shrDispl)

fosFracSum :: Expr
fosFracSum = sum1toN
  (inxi baseLngth * (inxi cohesion - inxi nrmStiffBase * inxi nrmDispl
    * tan(inxi fricAngle))) /
  sum1toN (inxi baseLngth * inxi shrStiffBase * inxi shrDispl)

rfemFoS_desc :: Sentence
rfemFoS_desc = foldlSent [
  (getES fsloc) `isThe` S "factor of safety for slice i.",
  (getES fs) `isThe` S "factor of safety for the entire slip surface.",
  (getES cohesion) `isThe` S "cohesion of slice i's base.",
  (getES fricAngle) `isThe` (phrase fricAngle), S "of slice i's base.",
  (getES nrmDispl) `isThe` S "normal displacement of slice i.",
  (getES shrDispl) `isThe` S "shear displacement of slice i.",
  (getES shrStiffBase) `isThe` S "length of the base of slice i.",
  (getES nrmStiffBase) `isThe` S "base normal stiffness at surface i.",
  (getES numbSlices) `isThe` S "number of slices in the slip surface"]

--
crtSlpId_new :: InstanceModel
crtSlpId_new = im' crtSlpId []
  [] (qw fs_min) [] "crtSlpId" [crtSlpId_desc]

crtSlpId :: RelationConcept
crtSlpId = makeRC "crtSlpId" (nounPhraseSP "critical slip identification")
  crtSlpId_desc crtSlpId_rel 

-- FIXME: horrible hack. This is short an argument... that was never defined!
crtSlpId_rel :: Relation
crtSlpId_rel = (sy fs_min) $= (apply1 minFunction critCoords) -- sy inputHack])
  --FIXME: add subscript to fs

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "Given the necessary", phrase slope,
  S "inputs, a minimization", S "algorithm or function", getES minFunction,
  S "will identify the", phrase crtSlpSrf, S "of the", phrase slope `sC`
  S "with the critical", phrase slip, S "coordinates", getES critCoords, 
  S "and the", phrase fs_min, E (sy fs_min), S "that results"]

-----------
-- Intro --
-----------

instModIntro1, instModIntro2 :: Contents

instModIntro1 = foldlSP [S "The", titleize morPrice,
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, at_start analysis, S "is performed by",
  S "breaking the assumed failure", phrase surface,
  S "into a series of vertical", plural slice, S "of" +:+. phrase mass,
  S "Static equilibrium analysis using two", phrase force,
  S "equilibrium, and one moment", phrase equation, S "as in" +:+. acroT 2,
  S "The", phrase problem, S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  acroT 3, S "so the", phrase assumption, S "of", acroGD 5,
  S "is used. Solving for", phrase force, S "equilibrium allows",
  plural definition, S "of all", plural force, S "in terms of the",
  plural physicalProperty, S "of", ddRef sliceWght, S "to",
  ddRef lengthLs `sC` S "as done in", ddRef seismicLoadF `sC` ddRef surfLoads]

instModIntro2 = foldlSP [
  plural value `ofThe'` (phrase intrslce +:+ phrase totNrmForce),
  getES intNormForce, S "the", getTandS normToShear `sC`
  S "and the", titleize fs, (sParen $ getES fs) `sC` S "are unknown.",
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", ddRef sliceWght, S "to", ddRef lengthLs `sC` S "the", plural value,
  S "of", getES shearRNoIntsl `sC` S "and", getES shearFNoIntsl, S "in",
  ddRef seismicLoadF, S "and", ddRef surfLoads `sC` S "and each",
  S "other. The relationships between the unknowns are non linear" `sC`
  S "and therefore explicit", plural equation, S "cannot be derived and an",
  S "iterative", plural solution, S "method is required"]

-----------------
-- Derivations --
-----------------

-- FIXEME: move derivations with the appropriate instance model

fctSfty_deriv_ssp :: Derivation
fctSfty_deriv_ssp = (weave [fctSfty_deriv_sentences_ssp, map E [fcSfty_rel]]) ++ fUnknowns_new

fctSfty_deriv_sentences_ssp :: [Sentence]
fctSfty_deriv_sentences_ssp = map foldlSentCol [fctSftyDerivation_new]

fctSftyDerivation_new :: [Sentence]
fctSftyDerivation_new = [S "Using", eqN 21, S "from", acroIM 3 `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", acroIM 1] -- ++ eqUnR fcSfty_rel ++ fUnknowns

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that",
  --FIXME: Index
  E (idx (sy intNormForce) 0) `sAnd`
  E (indxn intNormForce), S "are equal to", E 0]

fUnknowns_new :: [Sentence]
fUnknowns_new = [S "The constants" +:+ getES mobShrC `sAnd` getES shrResC +:+ 
  S "described in" +:+ eqN 20 `sAnd` eqN 19 +:+
  S "are functions of the unknowns: the" +:+ getTandS normToShear +:+
  sParen (acroIM 2) `andThe` getTandS fs +:+. sParen (acroIM 1)]


fUnknowns :: Contents
fUnknowns = foldlSP [S "The constants", getES mobShrC `sAnd` getES shrResC, 
  S "described in", eqN 20 `sAnd` eqN 19,
  S "are functions of the unknowns: the", getTandS normToShear,
  sParen (acroIM 2) `andThe` getTandS fs, sParen (acroIM 1)]

---------------------------------------------------------------------------
nrmShr_deriv_ssp :: Derivation
nrmShr_deriv_ssp = (weave [nrmShrDerivation_new, map E nrmShr_deriv_eqns_ssp]) ++ nrmShr_deriv_sentences_ssp_s4

nrmShr_deriv_sentences_ssp_s1 :: [Sentence]
nrmShr_deriv_sentences_ssp_s1 = [S "Taking the last static", phrase equation,
  S "of", acroT 2, S "with the", S "moment equilibrium" `sOf` acroGD 6,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", acroGD 5, S "results in", eqN 13]

nrmShr_deriv_sentences_ssp_s2 :: [Sentence]
nrmShr_deriv_sentences_ssp_s2 = [S "The", phrase equation, S "in terms of", getES normToShear,
  S "leads to", eqN 14]

nrmShr_deriv_sentences_ssp_s3 :: [Sentence]
nrmShr_deriv_sentences_ssp_s3 = [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", getES normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", acroIM 2]

nrmShr_deriv_sentences_ssp_s4 :: [Sentence]
nrmShr_deriv_sentences_ssp_s4 = [eqN 15 +:+ S "for" +:+ getES normToShear `sC`
  S "is a function of the unknown" +:+ getTandS intNormForce +:+. acroIM 3]


nrmShrDerivation_new :: [Sentence]
nrmShrDerivation_new = map foldlSentCol [nrmShr_deriv_sentences_ssp_s1, nrmShr_deriv_sentences_ssp_s2,
  nrmShr_deriv_sentences_ssp_s3]

nrmShr_deriv_eqns_ssp :: [Expr]
nrmShr_deriv_eqns_ssp = [eq1, eq2, eq3]

eq1, eq2, eq3:: Expr
eq1 = 0 $=
  momExpr (\ x y -> x - (sy normToShear * (inxi baseWthX / 2) * 
  (inxi intNormForce * inxi scalFunc + inxiM1 intNormForce *
  inxiM1 scalFunc)) + y)

eq2 = sy normToShear $= momExpr (+)
  / ((inxi baseWthX / 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc))

eq3 = inxi normToShear $= sum1toN
  (inxi baseWthX * (sy fixme1 + sy fixme2) * tan(inxi baseAngle) +
  inxi midpntHght * (sy earthqkLoadFctr * inxi slcWght -
  2 * inxi surfHydroForce * sin(inxi surfAngle) -
  2 * inxi surfLoad * sin(inxi impLoadAngle))) / 
  sum1toN
  (inxi baseWthX * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc))


---------------------------------------------------------------------------
intrSlc_deriv_ssp :: Derivation
intrSlc_deriv_ssp = weave [intrSlcDerivation_new, map E intrSlc_deriv_eqns_ssp] ++ fUnknowns_new

intrSlc_deriv_sentences_ssp_s1 :: [Sentence]
intrSlc_deriv_sentences_ssp_s1 = [S "Taking the", S "normal force equilibrium" `sOf` acroGD 1,
  S "with the", S "effective stress", phrase definition, S "from", acroT 4,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce $= inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", acroGD 5, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16]

intrSlc_deriv_sentences_ssp_s2 :: [Sentence]
intrSlc_deriv_sentences_ssp_s2 = [S "Taking the", S "base shear force equilibrium" `sOf`
  acroGD 2, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", acroGD 4 `sAnd`
  S "the assumption of", acroGD 5 `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17]

intrSlc_deriv_sentences_ssp_s3 :: [Sentence]
intrSlc_deriv_sentences_ssp_s3 = [S "Substituting the", phrase equation, S "for", getES nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18]

intrSlc_deriv_sentences_ssp_s4 :: [Sentence]
intrSlc_deriv_sentences_ssp_s4 = [S "Where", getES shearRNoIntsl `sAnd` getES shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, getES intNormForce `sAnd` getES intShrForce `sC`
  S "as defined in", ddRef resShearWO `sAnd` ddRef mobShearWO,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", acroIM 3]


intrSlcDerivation_new :: [Sentence]
intrSlcDerivation_new = map foldlSentCol [intrSlc_deriv_sentences_ssp_s1, intrSlc_deriv_sentences_ssp_s2,
  intrSlc_deriv_sentences_ssp_s3, intrSlc_deriv_sentences_ssp_s4]

intrSlc_deriv_eqns_ssp :: [Expr]
intrSlc_deriv_eqns_ssp = [eq4, eq5, eq6, eq7, eq8, eq9]

eq4, eq5, eq6, eq7, eq8, eq9:: Expr
eq4 = inxi nrmFSubWat $= eqlExpr cos sin (\x y -> x -
  sy normToShear * inxiM1 scalFunc * inxiM1 intNormForce + 
  sy normToShear * inxi scalFunc * inxi intNormForce + y)
  - (inxi baseHydroForce)

eq5 = ((inxi totNrmForce) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)) / (sy fs) $=
  --FIXME: pull the left side of this from GD4
  eqlExpr sin cos (\x y -> x - sy normToShear * inxiM1 scalFunc *
  inxiM1 intNormForce + sy normToShear * inxi scalFunc * inxi intNormForce + y)

eq6 = (inxi intNormForce) * (((sy normToShear)*(inxi scalFunc) *
  cos (inxi baseAngle) - sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs)) $= (inxiM1 intNormForce) *
  (((sy normToShear)*(inxiM1 scalFunc) * cos (inxi baseAngle)
  - sin (inxi baseAngle)) * tan (inxi fricAngle) - ((sy normToShear) *
  (inxiM1 scalFunc) * sin (inxi baseAngle) - cos (inxi baseAngle)) *
  (sy fs)) + (sy fs) * (inxi shearFNoIntsl) - (inxi shearRNoIntsl)

eq7 = (inxi shrResC) $= ((sy normToShear)*(inxi scalFunc) * cos (inxi baseAngle) -
  sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs)

eq8 = (inxi mobShrC) $= ((sy normToShear)*(inxi scalFunc) *
  cos (inxiP1 baseAngle) - sin (inxiP1 baseAngle)) *
  tan (inxi fricAngle) - ((sy normToShear)*(inxi scalFunc) *
  sin (inxiP1 baseAngle) - cos (inxiP1 baseAngle)) * (sy fs)

eq9 = (inxi intNormForce) $= (inxiM1 mobShrC * inxiM1 intNormForce +
  sy fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC


---------------------------------------------------------------------------
rigDis_deriv_ssp :: Derivation
rigDis_deriv_ssp = weave [rigDisDerivation_new, map E rigDis_deriv_eqns_ssp] ++ [foldlSentCol rigDis_deriv_sentences_ssp_s2]

rigDis_deriv_sentences_ssp_s1 :: [Sentence]
rigDis_deriv_sentences_ssp_s1 = [S "Using the net force-displacement equilibrium" +:+
  phrase equation +:+ S "of a slice from" +:+ ddRef netFDsplcmntEqbm +:+ S "with the" +:+ plural definition
  +:+ S "of the stiffness matrices" +:+ S "from" +:+ ddRef displcmntRxnF +:+ S "and the force" +:+
  plural definition +:+ S "from" +:+ acroGD 7 +:+ S "a broken down force displacement" +:+
  S "equilibrium" +:+ phrase equation +:+. S "can be derived" +:+ eqN 22 +:+
  S "gives the broken down" +:+ phrase equation +:+ S "in the" +:+ getES xi +:+
  S "direction" `sC` S "and" +:+ eqN 23 +:+ S "gives the broken down" +:+
  phrase equation +:+ S "in the" +:+ getES yi +:+ S "direction"]

rigDis_deriv_sentences_ssp_s2 :: [Sentence]
rigDis_deriv_sentences_ssp_s2 = [S "Using the known input assumption of" +:+ acroA 2 `sC`
  S "the force variable" +:+ plural definition +:+ S "of" +:+ ddRef sliceWght +:+ S "to" +:+
  ddRef surfLoads +:+ S "on the" +:+ S "left side of the" +:+ plural equation +:+
  S "can be solved for. The only unknown in the variables to solve" +:+
  S "for the stiffness values from" +:+ ddRef shearStiffness +:+. 
  S "is the displacements" +:+ S "Therefore taking the" +:+ phrase equation +:+ 
  S "from each slice a set of" +:+ (E $ 2 * sy numbSlices) +:+ plural equation
  `sC` S "with" +:+ (E $ 2 * sy numbSlices) +:+ S "unknown displacements in the" +:+ 
  getES xi `sAnd` getES yi +:+ S "directions of each slice can be derived." +:+.
  S "Solutions for the displacements of each slice can then be found" +:+
  S "The use of displacement in the" +:+ phrase definition +:+
  S "of the stiffness values makes the" +:+ phrase equation +:+ S "implicit, which means" +:+
  S "an iterative solution method, with an initial guess for the" +:+
  S "displacements in the stiffness" +:+ plural value +:+ S "is required"]


rigDisDerivation_new :: [Sentence]
rigDisDerivation_new = [foldlSentCol rigDis_deriv_sentences_ssp_s1]

rigDis_deriv_eqns_ssp :: [Expr]
rigDis_deriv_eqns_ssp = [fDisEq_rel]


---------------------------------------------------------------------------
rigFoS_deriv_ssp :: Derivation
rigFoS_deriv_ssp = rigFoS_deriv_sentences_ssp_s1 ++ [E eq10] ++ [E eq11] ++ weave [rigFoSDerivation_new, map E rigFoS_deriv_eqns_ssp]

rigFoS_deriv_sentences_ssp_s1 :: [Sentence]
rigFoS_deriv_sentences_ssp_s1 = [S "RFEM analysis can also be used to calculate the" +:+
  phrase fs +:+ S "for the" +:+. phrase slope +:+ S "For a slice element" +:+
  getES index +:+ S "the displacements" +:+ getES dx_i `sAnd` getES dy_i `sC` 
  S "are solved from the system of" +:+ plural equation +:+ S "in" +:+.
  acroIM 4 +:+ S "The" +:+ phrase definition +:+ S "of" +:+ getES rotatedDispl +:+
  S "as the" +:+ S "rotation of the displacement vector" +:+ getES genDisplace +:+
  S "is seen in" +:+. acroGD 9 +:+ S "This is used to find the" +:+
  plural displacement +:+ S "of the slice parallel to" +:+ S "the base of the slice" 
   +:+ getES shrDispl `sIn` eqN 24 +:+ S "and normal to" +:+ 
  S "the base of the slice" +:+ getES nrmDispl +:+ S "in" +:+. eqN 25]

rigFoS_deriv_sentences_ssp_s2 :: [Sentence]
rigFoS_deriv_sentences_ssp_s2 = [S "With the", phrase definition, S "of normal stiffness from",
  ddRef shearStiffness, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "the normal stiffness of the base", getES nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  getES nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+.
  acroT 5, S "Stress", getES normStress `sIs` S "used in place of",
  getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+.
  S "the length of the base", S "Results" `sIn` eqN 26]

rigFoS_deriv_sentences_ssp_s3 :: [Sentence]
rigFoS_deriv_sentences_ssp_s3 = [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", acroT 3,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27]

rigFoS_deriv_sentences_ssp_s4 :: [Sentence]
rigFoS_deriv_sentences_ssp_s4 = [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", getES normStress,
  S "from", eqN 26, S "and the", phrase definition,
  S "of displacement shear to the base", getES shrDispl, S "from",
  eqN 25 `sC` S "the value of", getES shrStiffBase, S "becomes solvable"]

rigFoS_deriv_sentences_ssp_s5 :: [Sentence]
rigFoS_deriv_sentences_ssp_s5 = [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", getES shrDispl, S "calculated in", eqN 24,
  --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn,
  getES shrStress, S "can be calculated using", acroT 5 `sC`
  S "as done in" +:+. eqN 29, S "Again, stress", getES shrStress,
  S "is used in place of force", getES genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for",
  S "length" `ofThe` S "base"]

rigFoS_deriv_sentences_ssp_s6 :: [Sentence]
rigFoS_deriv_sentences_ssp_s6 = [S "The", getTDS shrStress, S "acts as the mobile shear",
  S "acting on the base. Using the", phrase definition, titleize fs,
  phrase equation, S "from", acroT 1 `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice",
  getES mobStress, S "from", eqN 27, S "and", getTandS shrStress,
  S "from", eqN 29, S "the", getTandS fsloc,
  S "can be found from as seen in", eqN 30 `sAnd` acroIM 5]

rigFoS_deriv_sentences_ssp_s7 :: [Sentence]
rigFoS_deriv_sentences_ssp_s7 = [S "The global", titleize fs, S "is then", S "ratio" `ofThe`
  S "summation of the resistive and mobile shears for each slice" `sC`
  S "with a weighting for" +:+. (S "length" `ofThe` S "slice's base"),
  S "Shown in" +:+ eqN 31 `sAnd` acroIM 5]


rigFoSDerivation_new :: [Sentence]
rigFoSDerivation_new = map foldlSentCol [rigFoS_deriv_sentences_ssp_s2,
  rigFoS_deriv_sentences_ssp_s3, rigFoS_deriv_sentences_ssp_s4, rigFoS_deriv_sentences_ssp_s5,
   rigFoS_deriv_sentences_ssp_s6, rigFoS_deriv_sentences_ssp_s7]

rigFoS_deriv_eqns_ssp :: [Expr]
rigFoS_deriv_eqns_ssp = [eq12, eq13, eq14, eq15, eq16, eq17]

eq10, eq11, eq12, eq13, eq14, eq15, eq16, eq17:: Expr
eq10 = inxi shrDispl $= cos(inxi baseAngle) * inxi dx_i +
  sin(inxi baseAngle) * inxi dy_i

eq11 = (inxi nrmDispl $= negate (sin(inxi baseAngle)) * inxi dx_i +
    sin(inxi baseAngle) * inxi dy_i)

eq12 = inxi normStress $= inxi nrmStiffBase * inxi nrmDispl

eq13 = inxi mobStress $= inxi cohesion - inxi normStress * tan(inxi fricAngle)

eq14 = inxi shrStiffBase $= inxi intNormForce / (2 * (1 + inxi poissnsRatio)) *
  (dbl 0.1 / inxi baseWthX) +
  (inxi cohesion - inxi normStress * tan(inxi fricAngle)) /
  (abs (inxi shrDispl) + sy constant_a)

eq15 = inxi shrStress $= inxi shrStiffBase * inxi shrDispl

eq16 = sy fsloc $= inxi mobStress / inxi shrStress $= fosFracLoc

eq17 = (sy fs) $= sum1toN (inxi baseLngth * inxi mobStress) /
  sum1toN (inxi baseLngth * inxi shrStress) $= fosFracSum


fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", acroIM 3 `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", acroIM 1],
  
  eqUnR fcSfty_rel,
  
  fUnknowns]

nrmShrDerivation = [

  foldlSP [S "Taking the last static", phrase equation,
  S "of", acroT 2, S "with the", S "moment equilibrium" `sOf` acroGD 6,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", acroGD 5, S "results in", eqN 13],
  
  eqUnR $ 0 $=
  momExpr (\ x y -> x - (sy normToShear * (inxi baseWthX / 2) * 
  (inxi intNormForce * inxi scalFunc + inxiM1 intNormForce *
  inxiM1 scalFunc)) + y),
  
  foldlSP [S "The", phrase equation, S "in terms of", getES normToShear,
  S "leads to", eqN 14],
  
  eqUnR $
  sy normToShear $= momExpr (+)
  / ((inxi baseWthX / 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", getES normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", acroIM 2],
  --NOTE: "Taking this with that and the assumption of _
  --to get equation #" pattern
  
  eqUnR $
  inxi normToShear $= sum1toN
  (inxi baseWthX * (sy fixme1 + sy fixme2) * tan(inxi baseAngle) +
  inxi midpntHght * (sy earthqkLoadFctr * inxi slcWght -
  2 * inxi surfHydroForce * sin(inxi surfAngle) -
  2 * inxi surfLoad * sin(inxi impLoadAngle))) / 
  sum1toN
  (inxi baseWthX * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [eqN 15, S "for", getES normToShear `sC`
  S "is a function of the unknown", getTandS intNormForce, acroIM 3]

  ]

intrSlcDerivation = [

  foldlSP [S "Taking the", S "normal force equilibrium" `sOf` acroGD 1,
  S "with the", S "effective stress", phrase definition, S "from", acroT 4,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce $= inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", acroGD 5, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  eqUnR $
  inxi nrmFSubWat $= eqlExpr cos sin (\x y -> x -
  sy normToShear * inxiM1 scalFunc * inxiM1 intNormForce + 
  sy normToShear * inxi scalFunc * inxi intNormForce + y)
  - (inxi baseHydroForce),
  
  foldlSP [S "Taking the", S "base shear force equilibrium" `sOf`
  acroGD 2, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", acroGD 4 `sAnd`
  S "the assumption of", acroGD 5 `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17],
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  
  eqUnR $
  ((inxi totNrmForce) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)) / (sy fs) $=
  --FIXME: pull the left side of this from GD4
  eqlExpr sin cos (\x y -> x - sy normToShear * inxiM1 scalFunc *
  inxiM1 intNormForce + sy normToShear * inxi scalFunc * inxi intNormForce + y),
  
  foldlSP [S "Substituting the", phrase equation, S "for", getES nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18],

  eqUnR $
  (inxi intNormForce) * (((sy normToShear)*(inxi scalFunc) *
  cos (inxi baseAngle) - sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs)) $= (inxiM1 intNormForce) *
  (((sy normToShear)*(inxiM1 scalFunc) * cos (inxi baseAngle)
  - sin (inxi baseAngle)) * tan (inxi fricAngle) - ((sy normToShear) *
  (inxiM1 scalFunc) * sin (inxi baseAngle) - cos (inxi baseAngle)) *
  (sy fs)) + (sy fs) * (inxi shearFNoIntsl) - (inxi shearRNoIntsl),
  
  foldlSP [S "Where", getES shearRNoIntsl `sAnd` getES shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, getES intNormForce `sAnd` getES intShrForce `sC`
  S "as defined in", ddRef seismicLoadF `sAnd` ddRef surfLoads,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", acroIM 3],
  
  eqUnR $
  (inxi shrResC) $= ((sy normToShear)*(inxi scalFunc) * cos (inxi baseAngle) -
  sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs),
  -- FIXME: index everything here and add "Where i is the local
  -- slice of mass for 1 $<= i $<= n-1"
  eqUnR $
  (inxi mobShrC) $= ((sy normToShear)*(inxi scalFunc) *
  cos (inxiP1 baseAngle) - sin (inxiP1 baseAngle)) *
  tan (inxi fricAngle) - ((sy normToShear)*(inxi scalFunc) *
  sin (inxiP1 baseAngle) - cos (inxiP1 baseAngle)) * (sy fs),
  
  eqUnR $
  (inxi intNormForce) $= (inxiM1 mobShrC * inxiM1 intNormForce +
  sy fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC,
  
  fUnknowns]

rigDisDerivation = [
  
  foldlSP [S "Using the net force-displacement equilibrium",
  phrase equation, S "of a slice from", ddRef resShearWO, S "with", plural definition
  `ofThe` S "stiffness matrices", S "from", ddRef intrsliceF, S "and the force", 
  plural definition, S "from", acroGD 7 , S "a broken down force displacement",
  S "equilibrium", phrase equation +:+. S "can be derived", eqN 22,
  S "gives the broken down", phrase equation, S "in the", getES xi,
  S "direction" `sC` S "and", eqN 23, S "gives the broken down",
  phrase equation, S "in the", getES yi, S "direction"],

  eqUnR fDisEq_rel,
  
  foldlSP [S "Using the known input assumption of", (refA sspRefDB newA2) `sC`
  S "the force variable", plural definition, S "of", ddRef sliceWght, S "to",
  ddRef lengthLb, S "on", S "left side" `ofThe` plural equation,
  S "can be solved for. The only unknown in the variables to solve",
  S "for the stiffness values from", ddRef mobShearWO +:+. 
  S "is the displacements", S "Therefore taking the", phrase equation, 
  S "from each slice a set of", E $ 2 * sy numbSlices, plural equation
  `sC` S "with", E $ 2 * sy numbSlices, S "unknown displacements in the", 
  getES xi `sAnd` getES yi, S "directions of each slice can be derived.",
  S "Solutions for the displacements of each slice can then be found.",
  S "The use of displacement in", phrase definition `ofThe`
  S "stiffness values makes the", phrase equation, S "implicit, which means",
  S "an iterative solution method, with an initial guess for the",
  S "displacements in the stiffness", plural value, S "is required"]

  ]

rigFoSDerivation = [
  foldlSP [S "RFEM analysis can also be used to calculate the",
  phrase fs, S "for the" +:+. phrase slope, S "For a slice element",
  getES index, S "the displacements", getES dx_i `sAnd` getES dy_i `sC` 
  S "are solved from the system of", plural equation, S "in" +:+.
  acroIM 4, S "The", phrase definition, S "of", getES rotatedDispl,
  S "as", S "rotation" `ofThe` S "displacement vector", getES genDisplace,
  S "is seen in" +:+. acroGD 9, S "This is used to find",
  plural displacement `ofThe` S "slice parallel to the", S "base" `ofThe`
  S "slice", getES shrDispl `sIn` eqN 24, S "and normal to the", 
  S "base" `ofThe` S "slice", getES nrmDispl, S "in", eqN 25],
  
  eqUnR $ inxi shrDispl $= cos(inxi baseAngle) * inxi dx_i +
  sin(inxi baseAngle) * inxi dy_i,

  EqnBlock (inxi nrmDispl $= negate (sin(inxi baseAngle)) * inxi dx_i +
    sin(inxi baseAngle) * inxi dy_i) "",
  
  foldlSP [S "With the", phrase definition, S "of normal stiffness from",
  ddRef mobShearWO, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "normal stiffness" `ofThe` S "base", getES nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  getES nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+.
  acroT 5, S "Stress", getES normStress `sIs` S "used in place of",
  getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+.
  (S "length" `ofThe` S "base"), S "Results" `sIn` eqN 26],
  --FIXME: grammar

  eqUnR $
  inxi normStress $= inxi nrmStiffBase * inxi nrmDispl, --FIXME: index
  
  foldlSP [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", acroT 3,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27],
  
  eqUnR $
  inxi mobStress $= inxi cohesion - inxi normStress * tan(inxi fricAngle),
  --FIXME: index and prime
  
  foldlSP [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", getES normStress,
  S "from", eqN 26, S "and the", phrase definition,
  S "of displacement shear to the base", getES shrDispl, S "from",
  eqN 25 `sC` S "the value of", getES shrStiffBase, S "becomes solvable"],
  
  eqUnR $
  inxi shrStiffBase $= inxi intNormForce / (2 * (1 + inxi poissnsRatio)) *
  (dbl 0.1 / inxi baseWthX) +
  (inxi cohesion - inxi normStress * tan(inxi fricAngle)) /
  (abs (inxi shrDispl) + sy constant_a),
  
  foldlSP [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", getES shrDispl, S "calculated in", eqN 24,
  --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn,
  getES shrStress, S "can be calculated using", acroT 5 `sC`
  S "as done in" +:+. eqN 29, S "Again, stress", getES shrStress,
  S "is used in place of force", getES genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for",
  S "length" `ofThe` S "base"],
  
  eqUnR $
  inxi shrStress $= inxi shrStiffBase * inxi shrDispl,
  
  foldlSP [S "The", getTDS shrStress, S "acts as the mobile shear",
  S "acting on the base. Using the", phrase definition, titleize fs,
  phrase equation, S "from", acroT 1 `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice",
  getES mobStress, S "from", eqN 27, S "and", getTandS shrStress,
  S "from", eqN 29, S "the", getTandS fsloc,
  S "can be found from as seen in", eqN 30 `sAnd` acroIM 5],
  
  eqUnR $
  sy fsloc $= inxi mobStress / inxi shrStress $= fosFracLoc,
  
  foldlSP [S "The global", titleize fs, S "is then", S "ratio" `ofThe`
  S "summation of the resistive and mobile shears for each slice" `sC`
  S "with a weighting for" +:+. (S "length" `ofThe` S "slice's base"),
  S "Shown in", eqN 31 `sAnd` acroIM 5],
  
  eqUnR $
  (sy fs) $= sum1toN (inxi baseLngth * inxi mobStress) /
  sum1toN (inxi baseLngth * inxi shrStress) $= fosFracSum
  ]
