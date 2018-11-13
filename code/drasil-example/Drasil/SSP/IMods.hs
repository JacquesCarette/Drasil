module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil

import Data.Drasil.Utils (eqUnR', weave)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis, assumption, definition, 
  method_, physicalProperty, problem, solution, value)
import Data.Drasil.Concepts.Math (equation, surface)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (force)
import Data.Drasil.SentenceStructures (andThe, eqN, foldlSent, foldlSent_, 
  foldlSentCol, foldlSP, getTandS, isThe, ofThe, ofThe', sAnd, sOf)

import Drasil.SSP.Assumptions (newA2, newA4, newA10, newA11,newA6)
import Drasil.SSP.BasicExprs (eqlExpr, momExpr)
import Drasil.SSP.DataDefs (fixme1, fixme2,
  lengthLs, mobShearWO, resShearWO, seismicLoadF, sliceWght, 
  surfLoads)
import Drasil.SSP.GenDefs (normShrRGD, momentEqlGD, normForcEqGD)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, intrslce, morPrice, slice, slip, slope, ssa)
import Drasil.SSP.Labels (genDef1Label, genDef2Label, genDef4Label, genDef5Label, 
  genDef6Label)
import Drasil.SSP.References (chen2005, li2010)
import Drasil.SSP.TMods (equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseWthX, cohesion, 
  critCoords, earthqkLoadFctr, fricAngle, fs, fs_min, impLoadAngle, index, 
  indx1, indxn, intNormForce, intShrForce, inxi, inxiM1, inxiP1, midpntHght,
  minFunction, mobShrC, mobShrI, normFunc, normToShear, nrmFSubWat,
  numbSlices, scalFunc, shearFNoIntsl, shearFunc, shearRNoIntsl, 
  shrResC, slcWght, sum1toN, surfAngle, surfHydroForce, surfLoad, totNrmForce, 
  varblU, varblV, watrForce, wiif)

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [InstanceModel]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, crtSlpId]

--
fctSfty :: InstanceModel
fctSfty = im'' fctSfty_rc [qw shearRNoIntsl, qw shearFNoIntsl, qw mobShrC, qw shrResC, qw varblV]
  [] (qw fs) [] [makeRef chen2005] fctSftyDeriv "fctSfty" [fcSfty_desc]

fctSfty_rc :: RelationConcept
fctSfty_rc = makeRC "fctSfty_rc" factorOfSafety fcSfty_desc fcSfty_rel -- fctSftyL

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
fcSfty_desc = foldlSent_ [S "Equation for the", titleize fs `isThe` S "ratio",
  S "between resistive and mobile shear of the slip surface.",
  S "The sum of values from each slice is taken to find the total",
  S "resistive and mobile shear for the slip surface. The constants",
  ch shrResC, S "and", ch mobShrC, S "convert the resistive and",
  S "mobile shear without the inluence of" +:+.
  --FIXME: have these constants defined somewhere else
  S "interslice forces, to a calculation considering the interslice forces",
  makeRefS newA2, makeRefS newA6, makeRefS newA10, makeRefS newA11]

--
nrmShrFor :: InstanceModel
nrmShrFor = im'' nrmShrFor_rc [qw baseWthX, qw scalFunc,
 qw watrForce, qw baseAngle, qw midpntHght, 
 qw earthqkLoadFctr, qw slcWght, qw surfHydroForce]
  [sy fixme1 $< sy fixme1] (qw shearFunc)
   [0 $< sy fixme1 $< sy fixme1] [makeRef chen2005] nrmShrDeriv "nrmShrFor" [nrmShrF_desc]

nrmShrFor_rc :: RelationConcept
nrmShrFor_rc = makeRC "nrmShrFor_rc" (nounPhraseSP "normal/shear force ratio")
  nrmShrF_desc nrmShrF_rel -- nrmShrForL

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
  S "assumption of the Morgenstern Price method in", makeRefS genDef5Label,
  S "The inclination function", ch scalFunc,
  S "determines the relative magnitude ratio between the",
  S "different interslices, while", ch normToShear, S "determines the" +:+.
  S "magnitude", ch normToShear, S "uses the sum of interslice normal",
  S "and shear forces taken from each interslice"]

--

intsliceFs :: InstanceModel
intsliceFs = im'' intsliceFs_rc [qw index, qw fs, qw shearRNoIntsl, qw shearFNoIntsl,
 qw mobShrC, qw shrResC]
  [] (qw intNormForce) [] [makeRef chen2005] intrSlcDeriv "intsliceFs" [sliceFs_desc]

intsliceFs_rc :: RelationConcept
intsliceFs_rc = makeRC "intsliceFs_rc" (nounPhraseSP "interslice forces")
  sliceFs_desc sliceFs_rel -- inslideFxL

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
sliceFs_desc = foldlSent_ [S "The value of the interslice normal force",
  ch intNormForce, S "at interface", ch index +:+. S "The net force"
  `isThe` S "weight",
  S "of the slices adjacent to interface", ch index +:+.
  S "exert horizontally on each other", makeRefS newA2,
  makeRefS newA10, makeRefS newA11]

--
crtSlpId :: InstanceModel
crtSlpId = im' crtSlpId_rc [] [] (qw fs_min) [] [makeRef li2010]
  (mkLabelSame "crtSlpId" (Def Instance)) [crtSlpId_desc]

crtSlpId_rc :: RelationConcept
crtSlpId_rc = makeRC "crtSlpId_rc" (nounPhraseSP "critical slip identification")
  crtSlpId_desc crtSlpId_rel -- crtSlpIdL

-- FIXME: horrible hack. This is short an argument... that was never defined!
crtSlpId_rel :: Relation
crtSlpId_rel = (sy fs_min) $= (apply1 minFunction critCoords) -- sy inputHack])
  --FIXME: add subscript to fs

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent_ [S "Given the necessary", phrase slope,
  S "inputs, a minimization", S "algorithm or function", ch minFunction,
  S "will identify the", phrase crtSlpSrf, S "of the", phrase slope `sC`
  S "with the critical", phrase slip, S "coordinates", ch critCoords, 
  S "and the", phrase fs_min, E (sy fs_min) +:+. S "that results",
  makeRefS newA4]

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
  S "equilibrium, and one moment", phrase equation, S "as in" +:+. makeRefS equilibrium,
  S "The", phrase problem, S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  makeRefS mcShrStrgth, S "so the", phrase assumption, S "of", makeRefS genDef5Label,
  S "is used. Solving for", phrase force, S "equilibrium allows",
  plural definition, S "of all", plural force, S "in terms of the",
  plural physicalProperty, S "of", makeRefS sliceWght, S "to",
  makeRefS lengthLs `sC` S "as done in", makeRefS seismicLoadF `sC` makeRefS surfLoads]

instModIntro2 = foldlSP [
  plural value `ofThe'` (phrase intrslce +:+ phrase totNrmForce),
  ch intNormForce, S "the", getTandS normToShear `sC`
  S "and the", titleize fs, (sParen $ ch fs) `sC` S "are unknown.",
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", makeRefS sliceWght, S "to", makeRefS lengthLs `sC` S "the", plural value,
  S "of", ch shearRNoIntsl `sC` S "and", ch shearFNoIntsl, S "in",
  makeRefS seismicLoadF, S "and", makeRefS surfLoads `sC` S "and each",
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
fctSftyDerivationSentences = [S "Using", eqN 21, S "from", makeRefS intsliceFs `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", makeRefS fctSfty] -- ++ eqUnR' fcSfty_rel ++ fUnknowns

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that",
  --FIXME: Index
  E (idx (sy intNormForce) 0) `sAnd`
  E (indxn intNormForce), S "are equal to", E 0]

fUnknowns :: [Sentence]
fUnknowns = [S "The constants" +:+ ch mobShrC `sAnd` ch shrResC +:+ 
  S "described in" +:+ eqN 20 `sAnd` eqN 19 +:+
  S "are functions of the unknowns: the" +:+ getTandS normToShear +:+
  sParen (makeRefS nrmShrFor) `andThe` getTandS fs +:+. sParen (makeRefS fctSfty)]


fUnknownsCon :: Contents
fUnknownsCon = foldlSP fUnknowns

---------------------------------------------------------------------------
nrmShrDeriv :: Derivation
nrmShrDeriv = (weave [nrmShrDerivationSentences, map E nrmShrDerivEqns]) ++ nrmShrDerivSentence4

nrmShrDerivSentence1 :: [Sentence]
nrmShrDerivSentence1 = [S "From the moment equilibrium of", makeRefS momentEqlGD,
  S "with the primary assumption for the Morgenstern-Price method of", makeRefS newA6 `sAnd`
  S "associated definition", makeRefS normShrRGD, S "equation", eqN 9, S "can be derived"]

nrmShrDerivSentence2 :: [Sentence]
nrmShrDerivSentence2 = [S "Rearranging the", phrase equation, S "in terms of", ch normToShear,
  S "leads to", eqN 10]

nrmShrDerivSentence3 :: [Sentence]
nrmShrDerivSentence3 = [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "and removing the seismic and external forces due to", makeRefS newA10 `sAnd` makeRefS newA11
  `sC` S "a general", phrase equation, S "for the constant", ch normToShear,
  S "is developed in", eqN 11 `sC` S "also found in", makeRefS nrmShrFor]

nrmShrDerivSentence4 :: [Sentence]
nrmShrDerivSentence4 = [eqN 11 +:+ S "for" +:+ ch normToShear `sC`
  S "is a function of the unknown" +:+ getTandS intNormForce +:+. makeRefS intsliceFs]


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
intrSlcDerivSentence1 = [S "Substituting the", S "normal force equilibrium" `sOf`
  (makeRefS normForcEqGD) `sAnd` S "the assumption", makeRefS newA6,
  S "represented by", makeRefS momentEqlGD, 
  S "into the effective normal force definition from", makeRefS normShrRGD,
  S "yields equation", eqN 12] 

intrSlcDerivSentence2 :: [Sentence]
intrSlcDerivSentence2 = [S "Taking the", S "base shear force equilibrium" `sOf`
  makeRefS genDef2Label, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", makeRefS genDef4Label `sAnd`
  S "the assumption of", makeRefS genDef5Label `sC`
  S "the equilibrium", phrase equation,
  S "can be rewritten as", eqN 17]

intrSlcDerivSentence3 :: [Sentence]
intrSlcDerivSentence3 = [S "Substituting the", phrase equation, S "for", ch nrmFSubWat,
  S "from", eqN 16, makeRefS resShearWO `sAnd` makeRefS mobShearWO,
  S "into", eqN 17, S "and rearranging results in", eqN 18]

intrSlcDerivSentence4 :: [Sentence]
intrSlcDerivSentence4 = [S "Where", ch shearRNoIntsl `sAnd` ch shearFNoIntsl,
  S "are the resistive and mobile shear of the slice" `sC`
  S wiif, ch intNormForce `sAnd` ch intShrForce `sC`
  S "as defined in", makeRefS resShearWO `sAnd` makeRefS mobShearWO,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", makeRefS intsliceFs]


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

fctSftyDerivation, nrmShrDerivation, intrSlcDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", makeRefS intsliceFs `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", makeRefS fctSfty],
  
  eqUnR' fcSfty_rel,
  
  fUnknownsCon]

nrmShrDerivation = [

  foldlSP [S "The last static", phrase equation,
  S "of", makeRefS equilibrium, S "with the", S "moment equilibrium" `sOf` makeRefS genDef6Label,
  S "about", (S "midpoint" `ofThe` S "base") `sAnd` S "the",
  phrase assumption, S "of", makeRefS genDef5Label, S "results in", eqN 13],
  
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
  S "is developed in", eqN 15 `sC` S "also found in", makeRefS nrmShrFor],
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
  S "is a function of the unknown", getTandS intNormForce, makeRefS intsliceFs]

  ]

intrSlcDerivation = [

  foldlSP [S "Taking the", S "normal force equilibrium" `sOf` makeRefS genDef1Label,
  S "with the", S "effective stress", phrase definition, S "from", makeRefS effStress,
  -- NOTE: "Taking this with that and the assumption of _
  -- to get equation #" pattern
  S "that", E (inxi totNrmForce $= inxi nrmFSubWat - inxi baseHydroForce) `sC`
  S "and the assumption of", makeRefS genDef5Label, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  eqUnR' $
  inxi nrmFSubWat $= eqlExpr cos sin (\x y -> x -
  sy normToShear * inxiM1 scalFunc * inxiM1 intNormForce + 
  sy normToShear * inxi scalFunc * inxi intNormForce + y)
  - (inxi baseHydroForce),
  
  foldlSP [S "Taking the", S "base shear force equilibrium" `sOf`
  makeRefS genDef2Label, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", makeRefS genDef4Label `sAnd`
  S "the assumption of", makeRefS genDef5Label `sC`
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
  S "as defined in", makeRefS seismicLoadF `sAnd` makeRefS surfLoads,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", makeRefS intsliceFs],
  
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
