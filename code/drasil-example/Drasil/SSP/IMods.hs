module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)
import Control.Lens ((^.))

import Language.Drasil

import Data.Drasil.Utils (eqUnR', weave)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis, assumption, definition, 
  method_, physicalProperty, problem, solution, value)
import Data.Drasil.Concepts.Math (equation, surface)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (displacement, force)
import Data.Drasil.SentenceStructures (andThe, eqN, foldlSent, foldlSent_, 
  foldlSentCol, foldlSP, getTandS, getTDS, isThe, ofThe, ofThe', sAnd, sIn, sIs, sOf)

import Drasil.SSP.Assumptions (newA2)
import Drasil.SSP.BasicExprs (eqlExpr, momExpr)
import Drasil.SSP.DataDefs (displcmntRxnF, fixme1, fixme2, intrsliceF, lengthLb, 
  lengthLs, mobShearWO, netFDsplcmntEqbm, resShearWO, seismicLoadF, sliceWght, soilStiffness, 
  surfLoads)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, intrslce, morPrice, slice, slip, slope, ssa)
import Drasil.SSP.Labels (genDef1Label, genDef2Label, genDef4Label, genDef5Label, 
  genDef6Label, genDef7Label, genDef9Label, fctSftyL, nrmShrForL, inslideFxL, forDisEqlbL,
  rfemFoSL, crtSlpIdL)
import Drasil.SSP.References (chen2005, stolle2008, li2010)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress, hookesLaw)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, cohesion, 
  constant_a, critCoords, dx_i, dy_i, earthqkLoadFctr, effStiffA, effStiffB, fricAngle, 
  fs, fs_min, fsloc, genDisplace, genForce, impLoadAngle, index, indx1, indxn, intNormForce, 
  intShrForce, inxi, inxiM1, inxiP1, midpntHght, minFunction, mobShrC, mobShrI, mobStress, 
  normFunc, normStress, normToShear, nrmDispl, nrmFSubWat, nrmStiffBase, nrmStiffIntsl, 
  numbSlices, poissnsRatio, rotatedDispl, scalFunc, shearFNoIntsl, shearFunc, shearRNoIntsl, 
  shrDispl, shrResC, shrStiffBase, shrStiffIntsl, shrStress, slcWght, sum1toN, surfAngle, 
  surfHydroForce, surfLngth, surfLoad, totNrmForce, varblU, varblV, watrForce, watrForceDif, 
  wiif, xi, yi)

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [InstanceModel]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb,
 rfemFoS, crtSlpId]

--
fctSfty :: InstanceModel
fctSfty = im'' fctSfty_rc [qw shearRNoIntsl, qw shearFNoIntsl,
 qw mobShrC, qw shrResC, qw varblV]
  [] (qw fs) [] [makeRef chen2005] fctSftyDeriv "fctSfty" [fcSfty_desc]

fctSfty_rc :: RelationConcept
fctSfty_rc = makeRC "fctSfty_rc" factorOfSafety fcSfty_desc fcSfty_rel fctSftyL

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
  ch shrResC, S "and", ch mobShrC, S "convert the resistive and",
  S "mobile shear without the inluence of",
  --FIXME: have these constants defined somewhere else
  S "interslice forces, to a calculation considering the interslice forces"]

--
nrmShrFor :: InstanceModel
nrmShrFor = im'' nrmShrFor_rc [qw baseWthX, qw scalFunc,
 qw watrForce, qw baseAngle, qw midpntHght, 
 qw earthqkLoadFctr, qw slcWght, qw surfHydroForce]
  [TCon AssumedCon $ sy fixme1 $< sy fixme1] (qw shearFunc)
   [TCon AssumedCon $ 0 $< sy fixme1 $< sy fixme1] [makeRef chen2005] nrmShrDeriv "nrmShrFor" [nrmShrF_desc]

nrmShrFor_rc :: RelationConcept
nrmShrFor_rc = makeRC "nrmShrFor_rc" (nounPhraseSP "normal/shear force ratio")
  nrmShrF_desc nrmShrF_rel nrmShrForL

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
nrmShrF_desc = foldlSent [ch normToShear `isThe` S "magnitude ratio",
  S "between shear and normal forces at the interslice interfaces as the", 
  S "assumption of the Morgenstern Price method in", makeRef genDef5Label,
  S "The inclination function", ch scalFunc,
  S "determines the relative magnitude ratio between the",
  S "different interslices, while", ch normToShear, S "determines the" +:+.
  S "magnitude", ch normToShear, S "uses the sum of interslice normal",
  S "and shear forces taken from each interslice"]

--

intsliceFs :: InstanceModel
intsliceFs = im'' intsliceFs_rc [qw index, qw fs,
  qw shearRNoIntsl, qw shearFNoIntsl,
 qw mobShrC, qw shrResC]
  [] (qw intNormForce) [] [makeRef chen2005] intrSlcDeriv "intsliceFs" [sliceFs_desc]

intsliceFs_rc :: RelationConcept
intsliceFs_rc = makeRC "intsliceFs_rc" (nounPhraseSP "interslice forces")
  sliceFs_desc sliceFs_rel inslideFxL

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
  ch intNormForce, S "at interface", ch index +:+. S "The net force"
  `isThe` S "weight",
  S "of the slices adjacent to interface", ch index,
  S "exert horizontally on each other"]

--
forDisEqlb :: InstanceModel
forDisEqlb = im'' forDisEqlb_rc [qw baseAngle,
 qw baseHydroForce, qw surfHydroForce, qw surfAngle, qw surfLoad, qw impLoadAngle,
 qw surfLngth, qw nrmStiffIntsl, qw dx_i, qw effStiffA, qw dy_i, qw baseLngth, qw effStiffB]
  [] (qw earthqkLoadFctr) [] [makeRef stolle2008] rigDisDeriv "forDisEqlb" [fDisEq_desc']

forDisEqlb_rc :: RelationConcept
forDisEqlb_rc = makeRC "forDisEqlb_rc"
  (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel forDisEqlbL

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
  S "solved for displacements (", (ch dx_i), S "and", (ch dy_i), S ")"]

fDisEq_desc :: Sentence
fDisEq_desc = foldlSent [
  S "There is one set of force displacement equilibrium equations",
  S "in the x and y directions for each element. System of equations",
  S "solved for displacements (", (ch dx_i), S "and", (ch dy_i), S ")",
  (ch watrForceDif), S "=",
  (ch watrForce) `isThe` S "net hydrostatic force across a slice.", 
  (ch earthqkLoadFctr) `isThe` S "earthquake load factor.",
  (ch slcWght) `isThe` S "weight of the slice.",
  (ch baseHydroForce)  `isThe` S "pore water pressure acting on the",
  S "slice base.",
  (ch surfHydroForce) `isThe` S "pore water pressure acting on the",
  S "slice surface.",
  (ch baseAngle) `isThe` S "angle of the base with the horizontal.",
  (ch surfAngle) `isThe` S "angle of the surface with the horizontal.",
  (ch dx_i) `isThe` S "x displacement of slice i.",
  (ch dy_i) `isThe` S "y displacement of slice i.",
  (ch surfLngth) `isThe` S "length of the interslice surface i.",
  (ch baseLngth) `isThe` S "length of the base surface i.",
  (ch shrStiffIntsl) `isThe` S "interslice shear stiffness at surface i.",
  S " Kst,i-1" `isThe` S "interslice normal stiffness at surface i.",
  S "KbA,i, and KbB,i", S "are the base stiffness values for slice i"]

--
rfemFoS :: InstanceModel
rfemFoS = im''' rfemFoS_rc [qw cohesion, qw nrmStiffBase, qw nrmDispl,
 qw fricAngle, qw shrStiffBase, qw shrDispl, qw baseLngth]
  [] (qw fsloc) [] [makeRef stolle2008] rigFosDeriv "rfemFoS"


rfemFoS_rc :: RelationConcept
rfemFoS_rc = makeRC "rfemFoS_rc" (nounPhraseSP "RFEM factor of safety")
  rfemFoS_desc rfemFoS_rel rfemFoSL

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
  (ch fsloc) `isThe` S "factor of safety for slice i.",
  (ch fs) `isThe` S "factor of safety for the entire slip surface.",
  (ch cohesion) `isThe` S "cohesion of slice i's base.",
  (ch fricAngle) `isThe` (phrase fricAngle), S "of slice i's base.",
  (ch nrmDispl) `isThe` S "normal displacement of slice i.",
  (ch shrDispl) `isThe` S "shear displacement of slice i.",
  (ch shrStiffBase) `isThe` S "length of the base of slice i.",
  (ch nrmStiffBase) `isThe` S "base normal stiffness at surface i.",
  (ch numbSlices) `isThe` S "number of slices in the slip surface"]

--
crtSlpId :: InstanceModel
crtSlpId = im' crtSlpId_rc [] [] (qw fs_min) [] [makeRef li2010]
  (mkLabelSame "crtSlpId" (Def Instance)) [crtSlpId_desc]

crtSlpId_rc :: RelationConcept
crtSlpId_rc = makeRC "crtSlpId_rc" (nounPhraseSP "critical slip identification")
  crtSlpId_desc crtSlpId_rel crtSlpIdL

-- FIXME: horrible hack. This is short an argument... that was never defined!
crtSlpId_rel :: Relation
crtSlpId_rel = (sy fs_min) $= (apply1 minFunction critCoords) -- sy inputHack])
  --FIXME: add subscript to fs

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "Given the necessary", phrase slope,
  S "inputs, a minimization", S "algorithm or function", ch minFunction,
  S "will identify the", phrase crtSlpSrf, S "of the", phrase slope `sC`
  S "with the critical", phrase slip, S "coordinates", ch critCoords, 
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
  S "equilibrium, and one moment", phrase equation, S "as in" +:+. makeRef equilibrium,
  S "The", phrase problem, S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  makeRef mcShrStrgth, S "so the", phrase assumption, S "of", makeRef genDef5Label,
  S "is used. Solving for", phrase force, S "equilibrium allows",
  plural definition, S "of all", plural force, S "in terms of the",
  plural physicalProperty, S "of", makeRef sliceWght, S "to",
  makeRef lengthLs `sC` S "as done in", makeRef seismicLoadF `sC` makeRef surfLoads]

instModIntro2 = foldlSP [
  plural value `ofThe'` (phrase intrslce +:+ phrase totNrmForce),
  ch intNormForce, S "the", getTandS normToShear `sC`
  S "and the", titleize fs, (sParen $ ch fs) `sC` S "are unknown.",
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", makeRef sliceWght, S "to", makeRef lengthLs `sC` S "the", plural value,
  S "of", ch shearRNoIntsl `sC` S "and", ch shearFNoIntsl, S "in",
  makeRef seismicLoadF, S "and", makeRef surfLoads `sC` S "and each",
  S "other. The relationships between the unknowns are non linear" `sC`
  S "and therefore explicit", plural equation, S "cannot be derived and an",
  S "iterative", plural solution, S "method is required"]

-----------------
-- Derivations --
-----------------

-- FIXME: move derivations with the appropriate instance model

fctSftyDeriv :: Derivation
fctSftyDeriv = (weave [fctSftyDerivSentences, map E [fcSfty_rel]]) ++ fUnknowns

fctSftyDerivSentences :: [Sentence]
fctSftyDerivSentences = map foldlSentCol [fctSftyDerivationSentences]

fctSftyDerivationSentences :: [Sentence]
fctSftyDerivationSentences = [S "Using", eqN 21, S "from", makeRef intsliceFs `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", makeRef fctSfty] -- ++ eqUnR' fcSfty_rel ++ fUnknowns

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that",
  --FIXME: Index
  E (idx (sy intNormForce) 0) `sAnd`
  E (indxn intNormForce), S "are equal to", E 0]

fUnknowns :: [Sentence]
fUnknowns = [S "The constants" +:+ ch mobShrC `sAnd` ch shrResC +:+ 
  S "described in" +:+ eqN 20 `sAnd` eqN 19 +:+
  S "are functions of the unknowns: the" +:+ getTandS normToShear +:+
  sParen (makeRef nrmShrFor) `andThe` getTandS fs +:+. sParen (makeRef fctSfty)]


fUnknownsCon :: Contents
fUnknownsCon = foldlSP fUnknowns

---------------------------------------------------------------------------
nrmShrDeriv :: Derivation
nrmShrDeriv = (weave [nrmShrDerivationSentences, map E nrmShrDerivEqns]) ++ nrmShrDerivSentence4

nrmShrDerivSentence1 :: [Sentence]
nrmShrDerivSentence1 = [S "Taking the last static", phrase equation,
  S "of", makeRef equilibrium, S "with the", S "moment equilibrium" `sOf` makeRef genDef6Label,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", makeRef genDef5Label, S "results in", eqN 13]

nrmShrDerivSentence2 :: [Sentence]
nrmShrDerivSentence2 = [S "The", phrase equation, S "in terms of", ch normToShear,
  S "leads to", eqN 14]

nrmShrDerivSentence3 :: [Sentence]
nrmShrDerivSentence3 = [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", ch normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", makeRef nrmShrFor]

nrmShrDerivSentence4 :: [Sentence]
nrmShrDerivSentence4 = [eqN 15 +:+ S "for" +:+ ch normToShear `sC`
  S "is a function of the unknown" +:+ getTandS intNormForce +:+. makeRef intsliceFs]


nrmShrDerivationSentences :: [Sentence]
nrmShrDerivationSentences = map foldlSentCol [nrmShrDerivSentence1, nrmShrDerivSentence2,
  nrmShrDerivSentence3]

nrmShrDerivEqns :: [Expr]
nrmShrDerivEqns = [eq1, eq2, eq3]

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
intrSlcDeriv :: Derivation
intrSlcDeriv = weave [intrSlcDerivationSentences, map E intrSlcDerivEqns] ++ fUnknowns

intrSlcDerivSentence1 :: [Sentence]
intrSlcDerivSentence1 = [S "Taking the", S "normal force equilibrium" `sOf` makeRef genDef1Label,
  S "with the", S "effective stress", phrase definition, S "from", makeRef effStress,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce $= inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", makeRef genDef5Label, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16]

intrSlcDerivSentence2 :: [Sentence]
intrSlcDerivSentence2 = [S "Taking the", S "base shear force equilibrium" `sOf`
  makeRef genDef2Label, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", makeRef genDef4Label `sAnd`
  S "the assumption of", makeRef genDef5Label `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17]

intrSlcDerivSentence3 :: [Sentence]
intrSlcDerivSentence3 = [S "Substituting the", phrase equation, S "for", ch nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18]

intrSlcDerivSentence4 :: [Sentence]
intrSlcDerivSentence4 = [S "Where", ch shearRNoIntsl `sAnd` ch shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, ch intNormForce `sAnd` ch intShrForce `sC`
  S "as defined in", makeRef resShearWO `sAnd` makeRef mobShearWO,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", makeRef intsliceFs]


intrSlcDerivationSentences :: [Sentence]
intrSlcDerivationSentences = map foldlSentCol [intrSlcDerivSentence1, intrSlcDerivSentence2,
  intrSlcDerivSentence3, intrSlcDerivSentence4]

intrSlcDerivEqns :: [Expr]
intrSlcDerivEqns = [eq4, eq5, eq6, eq7, eq8, eq9]

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
rigDisDeriv :: Derivation
rigDisDeriv = weave [rigDisDerivationSentences, map E rigDisDerivEqns] ++ rigDisDerivSentence2

rigDisDerivSentence1 :: [Sentence]
rigDisDerivSentence1 = [S "Using the net force-displacement equilibrium" +:+
  phrase equation +:+ S "of a slice from" +:+ makeRef netFDsplcmntEqbm +:+ S "with the" +:+ plural definition
  +:+ S "of the stiffness matrices" +:+ S "from" +:+ makeRef displcmntRxnF +:+ S "and the force" +:+
  plural definition +:+ S "from" +:+ makeRef genDef7Label +:+ S "a broken down force displacement" +:+
  S "equilibrium" +:+ phrase equation +:+. S "can be derived" +:+ eqN 22 +:+
  S "gives the broken down" +:+ phrase equation +:+ S "in the" +:+ ch xi +:+
  S "direction" `sC` S "and" +:+ eqN 23 +:+ S "gives the broken down" +:+
  phrase equation +:+ S "in the" +:+ ch yi +:+ S "direction"]

rigDisDerivSentence2 :: [Sentence]
rigDisDerivSentence2 = [S "Using the known input assumption of" +:+ makeRef newA2 `sC`
  S "the force variable" +:+ plural definition +:+ S "of" +:+ makeRef sliceWght +:+ S "to" +:+
  makeRef surfLoads +:+ S "on the" +:+ S "left side of the" +:+ plural equation +:+
  S "can be solved for. The only unknown in the variables to solve" +:+
  S "for the stiffness values from" +:+ makeRef soilStiffness +:+. 
  S "is the displacements" +:+ S "Therefore taking the" +:+ phrase equation +:+ 
  S "from each slice a set of" +:+ (E $ 2 * sy numbSlices) +:+ plural equation
  `sC` S "with" +:+ (E $ 2 * sy numbSlices) +:+ S "unknown displacements in the" +:+ 
  ch xi `sAnd` ch yi +:+ S "directions of each slice can be derived." +:+.
  S "Solutions for the displacements of each slice can then be found" +:+
  S "The use of displacement in the" +:+ phrase definition +:+
  S "of the stiffness values makes the" +:+ phrase equation +:+ S "implicit, which means" +:+
  S "an iterative solution method, with an initial guess for the" +:+
  S "displacements in the stiffness" +:+ plural value +:+. S "is required"]


rigDisDerivationSentences :: [Sentence]
rigDisDerivationSentences = [foldlSentCol rigDisDerivSentence1]

rigDisDerivEqns :: [Expr]
rigDisDerivEqns = [fDisEq_rel]


---------------------------------------------------------------------------
rigFosDeriv :: Derivation
rigFosDeriv = rigFosDerivSentence1 ++ [E eq10] ++ [E eq11] ++ weave [rigFoSDerivationSentences, map E rigFosDerivEqns]

rigFosDerivSentence1 :: [Sentence]
rigFosDerivSentence1 = [S "RFEM analysis can also be used to calculate the" +:+
  phrase fs +:+ S "for the" +:+. phrase slope +:+ S "For a slice element" +:+
  ch index +:+ S "the displacements" +:+ ch dx_i `sAnd` ch dy_i `sC` 
  S "are solved from the system of" +:+ plural equation +:+ S "in" +:+.
  makeRef forDisEqlb +:+ S "The" +:+ phrase definition +:+ S "of" +:+ ch rotatedDispl +:+
  S "as the" +:+ S "rotation of the displacement vector" +:+ ch genDisplace +:+
  S "is seen in" +:+. makeRef genDef9Label +:+ S "This is used to find the" +:+
  plural displacement +:+ S "of the slice parallel to" +:+ S "the base of the slice" 
   +:+ ch shrDispl `sIn` eqN 24 +:+ S "and normal to" +:+ 
  S "the base of the slice" +:+ ch nrmDispl +:+ S "in" +:+. eqN 25]

rigFosDerivSentence2 :: [Sentence]
rigFosDerivSentence2 = [S "With the", phrase definition, S "of normal stiffness from",
  makeRef soilStiffness, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "the normal stiffness of the base", ch nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  ch nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+.
  makeRef hookesLaw, S "Stress", ch normStress `sIs` S "used in place of",
  getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+.
  S "the length of the base", S "Results" `sIn` eqN 26]

rigFosDerivSentence3 :: [Sentence]
rigFosDerivSentence3 = [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", makeRef mcShrStrgth,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27]

rigFosDerivSentence4 :: [Sentence]
rigFosDerivSentence4 = [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", ch normStress,
  S "from", eqN 26, S "and the", phrase definition,
  S "of displacement shear to the base", ch shrDispl, S "from",
  eqN 25 `sC` S "the value of", ch shrStiffBase, S "becomes solvable"]

rigFosDerivSentence5 :: [Sentence]
rigFosDerivSentence5 = [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", ch shrDispl, S "calculated in", eqN 24,
  --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn,
  ch shrStress, S "can be calculated using", makeRef hookesLaw `sC`
  S "as done in" +:+. eqN 29, S "Again, stress", ch shrStress,
  S "is used in place of force", ch genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for",
  S "length" `ofThe` S "base"]

rigFosDerivSentence6 :: [Sentence]
rigFosDerivSentence6 = [S "The", getTDS shrStress, S "acts as the mobile shear",
  S "acting on the base. Using the", phrase definition, titleize fs,
  phrase equation, S "from", makeRef factOfSafety `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice",
  ch mobStress, S "from", eqN 27, S "and", getTandS shrStress,
  S "from", eqN 29, S "the", getTandS fsloc,
  S "can be found from as seen in", eqN 30 `sAnd` makeRef rfemFoS]

rigFosDerivSentence7 :: [Sentence]
rigFosDerivSentence7 = [S "The global", titleize fs, S "is then", S "ratio" `ofThe`
  S "summation of the resistive and mobile shears for each slice" `sC`
  S "with a weighting for" +:+. (S "length" `ofThe` S "slice's base"),
  S "Shown in" +:+ eqN 31 `sAnd` makeRef rfemFoS]


rigFoSDerivationSentences :: [Sentence]
rigFoSDerivationSentences = map foldlSentCol [rigFosDerivSentence2,
  rigFosDerivSentence3, rigFosDerivSentence4, rigFosDerivSentence5,
   rigFosDerivSentence6, rigFosDerivSentence7]

rigFosDerivEqns :: [Expr]
rigFosDerivEqns = [eq12, eq13, eq14, eq15, eq16, eq17]

eq10, eq11, eq12, eq13, eq14, eq15, eq16, eq17 :: Expr
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

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", makeRef intsliceFs `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", makeRef fctSfty],
  
  eqUnR' fcSfty_rel,
  
  fUnknownsCon]

nrmShrDerivation = [

  foldlSP [S "The last static", phrase equation,
  S "of", makeRef equilibrium, S "with the", S "moment equilibrium" `sOf` makeRef genDef6Label,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", makeRef genDef5Label, S "results in", eqN 13],
  
  eqUnR' $ 0 $=
  momExpr (\ x y -> x - (sy normToShear * (inxi baseWthX / 2) * 
  (inxi intNormForce * inxi scalFunc + inxiM1 intNormForce *
  inxiM1 scalFunc)) + y),
  
  foldlSP [S "The", phrase equation, S "in terms of", ch normToShear,
  S "leads to", eqN 14],
  
  eqUnR' $
  sy normToShear $= momExpr (+)
  / ((inxi baseWthX / 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", ch normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", makeRef nrmShrFor],
  --NOTE: "Taking this with that and the assumption of _
  --to get equation #" pattern
  
  eqUnR' $
  inxi normToShear $= sum1toN
  (inxi baseWthX * (sy fixme1 + sy fixme2) * tan(inxi baseAngle) +
  inxi midpntHght * (sy earthqkLoadFctr * inxi slcWght -
  2 * inxi surfHydroForce * sin(inxi surfAngle) -
  2 * inxi surfLoad * sin(inxi impLoadAngle))) / 
  sum1toN
  (inxi baseWthX * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc)),
  
  foldlSP [eqN 15, S "for", ch normToShear `sC`
  S "is a function of the unknown", getTandS intNormForce, makeRef intsliceFs]

  ]

intrSlcDerivation = [

  foldlSP [S "Taking the", S "normal force equilibrium" `sOf` makeRef genDef1Label,
  S "with the", S "effective stress", phrase definition, S "from", makeRef effStress,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce $= inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", makeRef genDef5Label, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  eqUnR' $
  inxi nrmFSubWat $= eqlExpr cos sin (\x y -> x -
  sy normToShear * inxiM1 scalFunc * inxiM1 intNormForce + 
  sy normToShear * inxi scalFunc * inxi intNormForce + y)
  - (inxi baseHydroForce),
  
  foldlSP [S "Taking the", S "base shear force equilibrium" `sOf`
  makeRef genDef2Label, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", makeRef genDef4Label `sAnd`
  S "the assumption of", makeRef genDef5Label `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17],
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  
  eqUnR' $
  ((inxi totNrmForce) * tan (inxi fricAngle) + (inxi cohesion) *
  (inxi baseWthX) * sec (inxi baseAngle)) / (sy fs) $=
  --FIXME: pull the left side of this from GD4
  eqlExpr sin cos (\x y -> x - sy normToShear * inxiM1 scalFunc *
  inxiM1 intNormForce + sy normToShear * inxi scalFunc * inxi intNormForce + y),
  
  foldlSP [S "Substituting the", phrase equation, S "for", ch nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18],

  eqUnR' $
  (inxi intNormForce) * (((sy normToShear)*(inxi scalFunc) *
  cos (inxi baseAngle) - sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs)) $= (inxiM1 intNormForce) *
  (((sy normToShear)*(inxiM1 scalFunc) * cos (inxi baseAngle)
  - sin (inxi baseAngle)) * tan (inxi fricAngle) - ((sy normToShear) *
  (inxiM1 scalFunc) * sin (inxi baseAngle) - cos (inxi baseAngle)) *
  (sy fs)) + (sy fs) * (inxi shearFNoIntsl) - (inxi shearRNoIntsl),
  
  foldlSP [S "Where", ch shearRNoIntsl `sAnd` ch shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, ch intNormForce `sAnd` ch intShrForce `sC`
  S "as defined in", makeRef seismicLoadF `sAnd` makeRef surfLoads,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", makeRef intsliceFs],
  
  eqUnR' $
  (inxi shrResC) $= ((sy normToShear)*(inxi scalFunc) * cos (inxi baseAngle) -
  sin (inxi baseAngle)) * tan (inxi fricAngle) -
  ((sy normToShear)*(inxi scalFunc) * sin (inxi baseAngle) -
  cos (inxi baseAngle)) * (sy fs),
  -- FIXME: index everything here and add "Where i is the local
  -- slice of mass for 1 $<= i $<= n-1"
  eqUnR' $
  (inxi mobShrC) $= ((sy normToShear)*(inxi scalFunc) *
  cos (inxiP1 baseAngle) - sin (inxiP1 baseAngle)) *
  tan (inxi fricAngle) - ((sy normToShear)*(inxi scalFunc) *
  sin (inxiP1 baseAngle) - cos (inxiP1 baseAngle)) * (sy fs),
  
  eqUnR' $
  (inxi intNormForce) $= (inxiM1 mobShrC * inxiM1 intNormForce +
  sy fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC,
  
  fUnknownsCon]

rigDisDerivation = [
  
  foldlSP [S "Using the net force-displacement equilibrium",
  phrase equation, S "of a slice from", makeRef resShearWO, S "with", plural definition
  `ofThe` S "stiffness matrices", S "from", makeRef intrsliceF, S "and the force", 
  plural definition, S "from", makeRef genDef7Label , S "a broken down force displacement",
  S "equilibrium", phrase equation +:+. S "can be derived", eqN 22,
  S "gives the broken down", phrase equation, S "in the", ch xi,
  S "direction" `sC` S "and", eqN 23, S "gives the broken down",
  phrase equation, S "in the", ch yi, S "direction"],

  eqUnR' fDisEq_rel,
  
  foldlSP [S "Using the known input assumption of", (makeRef newA2) `sC`
  S "the force variable", plural definition, S "of", makeRef sliceWght, S "to",
  makeRef lengthLb, S "on", S "left side" `ofThe` plural equation,
  S "can be solved for. The only unknown in the variables to solve",
  S "for the stiffness values from", makeRef mobShearWO +:+. 
  S "is the displacements", S "Therefore taking the", phrase equation, 
  S "from each slice a set of", E $ 2 * sy numbSlices, plural equation
  `sC` S "with", E $ 2 * sy numbSlices, S "unknown displacements in the", 
  ch xi `sAnd` ch yi, S "directions of each slice can be derived.",
  S "Solutions for the displacements of each slice can then be found.",
  S "The use of displacement in", phrase definition `ofThe`
  S "stiffness values makes the", phrase equation, S "implicit, which means",
  S "an iterative solution method, with an initial guess for the",
  S "displacements in the stiffness", plural value, S "is required"]

  ]

rigFoSDerivation = [
  foldlSP [S "RFEM analysis can also be used to calculate the",
  phrase fs, S "for the" +:+. phrase slope, S "For a slice element",
  ch index, S "the displacements", ch dx_i `sAnd` ch dy_i `sC` 
  S "are solved from the system of", plural equation, S "in" +:+.
  makeRef forDisEqlb, S "The", phrase definition, S "of", ch rotatedDispl,
  S "as", S "rotation" `ofThe` S "displacement vector", ch genDisplace,
  S "is seen in" +:+. makeRef genDef9Label, S "This is used to find",
  plural displacement `ofThe` S "slice parallel to the", S "base" `ofThe`
  S "slice", ch shrDispl `sIn` eqN 24, S "and normal to the", 
  S "base" `ofThe` S "slice", ch nrmDispl, S "in", eqN 25],
  
  eqUnR' $ inxi shrDispl $= cos(inxi baseAngle) * inxi dx_i +
  sin(inxi baseAngle) * inxi dy_i,

  eqUnR' $ (inxi nrmDispl $= negate (sin(inxi baseAngle)) * inxi dx_i +
    sin(inxi baseAngle) * inxi dy_i),
  
  foldlSP [S "With the", phrase definition, S "of normal stiffness from",
  makeRef mobShearWO, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "normal stiffness" `ofThe` S "base", ch nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  ch nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+.
  makeRef hookesLaw, S "Stress", ch normStress `sIs` S "used in place of",
  getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+.
  (S "length" `ofThe` S "base"), S "Results" `sIn` eqN 26],
  --FIXME: grammar

  eqUnR' $
  inxi normStress $= inxi nrmStiffBase * inxi nrmDispl, --FIXME: index
  
  foldlSP [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", makeRef mcShrStrgth,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27],
  
  eqUnR' $
  inxi mobStress $= inxi cohesion - inxi normStress * tan(inxi fricAngle),
  --FIXME: index and prime
  
  foldlSP [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", ch normStress,
  S "from", eqN 26, S "and the", phrase definition,
  S "of displacement shear to the base", ch shrDispl, S "from",
  eqN 25 `sC` S "the value of", ch shrStiffBase, S "becomes solvable"],
  
  eqUnR' $
  inxi shrStiffBase $= inxi intNormForce / (2 * (1 + inxi poissnsRatio)) *
  (dbl 0.1 / inxi baseWthX) +
  (inxi cohesion - inxi normStress * tan(inxi fricAngle)) /
  (abs (inxi shrDispl) + sy constant_a),
  
  foldlSP [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", ch shrDispl, S "calculated in", eqN 24,
  --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn,
  ch shrStress, S "can be calculated using", makeRef hookesLaw `sC`
  S "as done in" +:+. eqN 29, S "Again, stress", ch shrStress,
  S "is used in place of force", ch genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for",
  S "length" `ofThe` S "base"],
  
  eqUnR' $
  inxi shrStress $= inxi shrStiffBase * inxi shrDispl,
  
  foldlSP [S "The", getTDS shrStress, S "acts as the mobile shear",
  S "acting on the base. Using the", phrase definition, titleize fs,
  phrase equation, S "from", makeRef factOfSafety `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice",
  ch mobStress, S "from", eqN 27, S "and", getTandS shrStress,
  S "from", eqN 29, S "the", getTandS fsloc,
  S "can be found from as seen in", eqN 30 `sAnd` makeRef rfemFoS],
  
  eqUnR' $
  sy fsloc $= inxi mobStress / inxi shrStress $= fosFracLoc,
  
  foldlSP [S "The global", titleize fs, S "is then", S "ratio" `ofThe`
  S "summation of the resistive and mobile shears for each slice" `sC`
  S "with a weighting for" +:+. (S "length" `ofThe` S "slice's base"),
  S "Shown in", eqN 31 `sAnd` makeRef rfemFoS],
  
  eqUnR' $
  (sy fs) $= sum1toN (inxi baseLngth * inxi mobStress) /
  sum1toN (inxi baseLngth * inxi shrStress) $= fosFracSum
  ]
