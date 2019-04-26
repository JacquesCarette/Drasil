module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil

import Data.Drasil.Utils (weave)

import Drasil.DocLang (goalStmt_label)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis, assumption, constraint,
  definition, first, goal, method_, physical, physicalProperty, problem, 
  solution, value)
import Data.Drasil.Concepts.Math (angle, equation, surface)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (force)
import Data.Drasil.SentenceStructures (andThe, eqN, foldlSent, foldlSent_, 
  foldlSentCol, foldlSP, getTandS, ofThe', sAnd, sOf)

import Drasil.SSP.Assumptions (assumpSSC, assumpINSFL, 
  assumpES, assumpSF, assumpSL)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, eqlExprSepG, eqlExprNSepG,   
  eqlExprNoKQ, eqlExprNNoKQ, sliceExpr, momExpr, momExprNoKQ)
import Drasil.SSP.DataCons (data_constraint_Table3)
import Drasil.SSP.DataDefs (convertFunc1, convertFunc2, lengthLs, sliceWght, 
  surfWtrF, intersliceWtrF, lengthB, angleA, angleB, slcHeight, ratioVariation)
import Drasil.SSP.GenDefs (normShrRGD, momentEqlGD, normForcEqGD, mobShearWOGD, 
  resShearWOGD, bsShrFEqGD, mobShrGD)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, intrslce, morPrice, slice, 
  slip, slope, slpSrf, ssa)
import Drasil.SSP.References (chen2005, li2010, karchewski2012)
import Drasil.SSP.TMods (equilibrium, mcShrStrgth)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX, 
  critCoords,
  effCohesion, constF, dryWeight, earthqkLoadFctr, fricAngle, fs, fs_min, 
  index, indx1, indxn, intNormForce, intShrForce, inxi, inxiM1, midpntHght,
  minFunction, mobShrC, mobShrI, nrmForceSum, nrmShearNum, normToShear, 
  nrmFSubWat, numbSlices, satWeight, scalFunc, shearFNoIntsl, nrmShearDen, 
  shearRNoIntsl, shrResC, slipDist, slipHght, slopeDist, slopeHght, sum1toN, 
  surfAngle, surfHydroForce, surfLoad, totNrmForce, varblV, watrForce, 
  waterDist, waterHght, waterWeight, watForceSum, xi, xMaxExtSlip, xMaxEtrSlip,
  xMinExtSlip, xMinEtrSlip, yi, yMaxSlip, yMinSlip)

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [InstanceModel]
sspIMods = [fctSfty, nrmShrFor, nrmShrForNum, nrmShrForDen, intsliceFs, crtSlpId]

--

fctSfty :: InstanceModel
fctSfty = im'' fctSfty_rc [qw slopeDist, qw slopeHght, qw waterHght, qw effCohesion, qw fricAngle, qw dryWeight, qw satWeight, qw waterWeight, qw slipDist, qw slipHght, qw constF]
  [] (qw fs) [] [chen2005, karchewski2012] fctSftyDeriv "fctSfty" [fcSfty_desc]

fctSfty_rc :: RelationConcept
fctSfty_rc = makeRC "fctSfty_rc" factorOfSafety fcSfty_desc fcSfty_rel -- fctSftyL

fcSfty_rel :: Relation
fcSfty_rel = sy fs $= sumOp shearRNoIntsl / sumOp shearFNoIntsl
  where prodOp = defprod (eqSymb varblV) (sy index) (sy numbSlices - 1)
          (idx (sy mobShrC) (sy varblV))
        sumOp sym = (defsum (eqSymb index) 1 (sy numbSlices - 1)
          (idx (sy sym) (sy index) * prodOp)) + idx (sy sym) (sy numbSlices)

fcSfty_desc :: Sentence
fcSfty_desc = foldlSent_ [ch shearRNoIntsl, S "is defined in", makeRef2S 
  resShearWOGD `sC` ch mobShrC, S "is defined in", makeRef2S convertFunc2 `sC` 
  S "and", ch shearFNoIntsl, S "is defined in", makeRef2S mobShearWOGD]

fctSftyDeriv :: Derivation
fctSftyDeriv = (weave [fctSftyDerivSentences1, map E fctSftyDerivEqns1]) ++
  [E fctSftyDerivEqn10b, E fctSftyDerivEqn10c, fctSftyDerivEllipsis,
  E fctSftyDerivEqn10d, E fctSftyDerivEqn10e, E fctSftyDerivEqn10f] ++ 
  (weave [fctSftyDerivSentences2, map E fctSftyDerivEqns2]) ++  
  fctSftyDerivSentence20

fctSftyDerivSentences1 :: [Sentence]
fctSftyDerivSentences1 = map foldlSentCol [fctSftyDerivSentence1,
  fctSftyDerivSentence2, fctSftyDerivSentence3, fctSftyDerivSentence4,
  fctSftyDerivSentence5, fctSftyDerivSentence6, fctSftyDerivSentence7,
  fctSftyDerivSentence8, fctSftyDerivSentence9, fctSftyDerivSentence10]

fctSftyDerivSentences2 :: [Sentence]
fctSftyDerivSentences2 = map foldlSentCol [fctSftyDerivSentence11,
  fctSftyDerivSentence12, fctSftyDerivSentence13, fctSftyDerivSentence14,
  fctSftyDerivSentence15, fctSftyDerivSentence16, fctSftyDerivSentence17,
  fctSftyDerivSentence18, fctSftyDerivSentence19]

fctSftyDerivEqns1 :: [Expr]
fctSftyDerivEqns1 = [fctSftyDerivEqn1, fctSftyDerivEqn2, fctSftyDerivEqn3,
  fctSftyDerivEqn4, fctSftyDerivEqn5, fctSftyDerivEqn6, fctSftyDerivEqn7,
  fctSftyDerivEqn8, fctSftyDerivEqn9, fctSftyDerivEqn10a]

fctSftyDerivEqns2 :: [Expr]
fctSftyDerivEqns2 = [fctSftyDerivEqn11, fctSftyDerivEqn12, fctSftyDerivEqn13,
  fctSftyDerivEqn14, fctSftyDerivEqn15, fctSftyDerivEqn16, fctSftyDerivEqn17,
  fctSftyDerivEqn18, fcSfty_rel]

fctSftyDerivSentence1 :: [Sentence]
fctSftyDerivSentence1 = [S "The" +:+ phrase mobShrI +:+ S "defined in",
  makeRef2S bsShrFEqGD, S "can be substituted into the definition of" +:+ 
  phrase mobShrI +:+ S "based on the" +:+ phrase fs `sC` S "from",
  makeRef2S mobShrGD, S "yielding", eqN 1, S "below"]

fctSftyDerivSentence2 :: [Sentence]
fctSftyDerivSentence2 = [S "An expression for the" +:+ phrase nrmFSubWat `sC`
  ch nrmFSubWat `sC` S "can be derived by substituting the" +:+
  phrase totNrmForce +:+ S "equilibrium from", makeRef2S normForcEqGD, 
  S "into the definition for" +:+ phrase nrmFSubWat +:+ S "from" +:+.
  makeRef2S resShearWOGD, S "This results in", eqN 2]

fctSftyDerivSentence3 :: [Sentence]
fctSftyDerivSentence3 = [S "Substituting", eqN 2, S "into", eqN 1, S "gives"]

fctSftyDerivSentence4 :: [Sentence]
fctSftyDerivSentence4 = [S "Since the" +:+ phrase intShrForce +:+
  ch intShrForce +:+ S "and" +:+ phrase intNormForce +:+ ch intNormForce +:+
  S "are unknown, they are separated from the other terms as follows"]

fctSftyDerivSentence5 :: [Sentence]
fctSftyDerivSentence5 = [S "Applying assumptions" +:+ makeRef2S assumpSF `sAnd`
  makeRef2S assumpSL `sC` S "which state that the" +:+
  phrase earthqkLoadFctr `andThe` phrase surfLoad `sC`
  S "respectively, are zero, allows for further simplification as shown below"]

fctSftyDerivSentence6 :: [Sentence]
fctSftyDerivSentence6 = [S "The definitions of" +:+ makeRef2S resShearWOGD 
  `sAnd` makeRef2S mobShearWOGD +:+ S "are present in this equation, and" +:+
  S "thus can be replaced by" +:+ E (inxi shearRNoIntsl) `sAnd`
  E (inxi shearFNoIntsl) `sC` S "respectively"]

fctSftyDerivSentence7 :: [Sentence]
fctSftyDerivSentence7 = [S "The" +:+ phrase intShrForce +:+ ch intShrForce +:+
  S "can be expressed in terms of the" +:+ phrase intNormForce +:+
  ch intNormForce +:+ S "using" +:+ makeRef2S assumpINSFL `sAnd`
  makeRef2S normShrRGD `sC` S "resulting in"]

fctSftyDerivSentence8 :: [Sentence]
fctSftyDerivSentence8 = [S "Rearranging yields the following"]

fctSftyDerivSentence9 :: [Sentence]
fctSftyDerivSentence9 = [S "The definitions for" +:+ ch shrResC `sAnd`
  ch mobShrC +:+ S "from" +:+ makeRef2S convertFunc1 `sAnd` 
  makeRef2S convertFunc2 +:+ S "simplify the above to", eqN 3]

fctSftyDerivSentence10 :: [Sentence]
fctSftyDerivSentence10 = [S "Versions of", eqN 3, S "instantiated for slices",
  S "1 to", ch numbSlices, S "are shown below"]

fctSftyDerivEllipsis :: Sentence
fctSftyDerivEllipsis = S "..."

fctSftyDerivSentence11 :: [Sentence]
fctSftyDerivSentence11 = [S "Applying", makeRef2S assumpES `sC`
  S "which says that", E (idx (sy intNormForce) 0) `sAnd`
  E (indxn intNormForce), S "are zero, results in the following special cases:",eqN 8, S "for the first slice"]

fctSftyDerivSentence12 :: [Sentence]
fctSftyDerivSentence12 = [S "and", eqN 9, S "for the", ch numbSlices :+:
  S "th slice"]

fctSftyDerivSentence13 :: [Sentence]
fctSftyDerivSentence13 = [S "Substituting", eqN 8, S "into", eqN 4, S "yields",
  eqN 10]

fctSftyDerivSentence14 :: [Sentence]
fctSftyDerivSentence14 = [S "which can be substituted into", eqN 5, S "to get",   eqN 11]

fctSftyDerivSentence15 :: [Sentence]
fctSftyDerivSentence15 = [S "and so on until", eqN 12, S "is obtained from",   eqN 7]

fctSftyDerivSentence16 :: [Sentence]
fctSftyDerivSentence16 = [eqN 9, S "can then be substituted into the left-hand",
  S "side of", eqN 12 `sC` S "resulting in"]

fctSftyDerivSentence17 :: [Sentence]
fctSftyDerivSentence17 = [S "This can be rearranged by multiplying boths sides",
  S "by", E (idx (sy mobShrC) (sy numbSlices - int 1)) `sAnd`
  S "then distributing the multiplication of each", ch mobShrC,
  S "over addition to obtain"]

fctSftyDerivSentence18 :: [Sentence]
fctSftyDerivSentence18 = [S "The multiplication of the", ch mobShrC,
  S "terms can be further distributed over the subtractions, resulting in the",
  S "equation having terms that each either contain an", ch shearRNoIntsl,
  S "or a" +:+. ch shearFNoIntsl, S "The equation can then be rearranged so",
  S "terms containing an", ch shearRNoIntsl, S "are on one side of the",
  S "equality, and terms containing a", ch shearFNoIntsl +:+.
  S "are on the other", S "The multiplication by the", phrase fs,
  S "is common to all of the", ch shearFNoIntsl, S "terms, and thus can be",
  S "factored out, resulting in"]

fctSftyDerivSentence19 :: [Sentence]
fctSftyDerivSentence19 = [S "Isolating the", phrase fs, S "on the left-hand",
  S "side and using compact notation for the products and sums yields",
  eqN 13 `sC` S "which can also be seen in", makeRef2S fctSfty]

fctSftyDerivSentence20 :: [Sentence]
fctSftyDerivSentence20 = [ch fs +:+ S "depends on the unknowns" +:+ 
  ch normToShear +:+ sParen (makeRef2S nrmShrFor) `sAnd` ch intNormForce +:+.
  sParen (makeRef2S intsliceFs)]

fctSftyDerivEqn1 :: Expr
fctSftyDerivEqn1 = --FIXME: pull the right side of this from GD4
  eqlExpr sin cos (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)
  $= ((inxi nrmFSubWat) * tan (sy fricAngle) + (sy effCohesion) *
  (inxi baseLngth)) / (sy fs)

fctSftyDerivEqn2 :: Expr
fctSftyDerivEqn2 = inxi nrmFSubWat $= eqlExprN cos sin (\x y -> x -
  inxiM1 intShrForce + inxi intShrForce + y) - (inxi baseHydroForce)

fctSftyDerivEqn3 :: Expr
fctSftyDerivEqn3 = eqlExpr sin cos (\x y -> x - inxiM1 intShrForce + 
  inxi intShrForce + y) $= ((eqlExprN cos sin (\x y -> x -
  inxiM1 intShrForce + inxi intShrForce + y) - (inxi baseHydroForce)) * 
  tan (sy fricAngle) + (sy effCohesion) * (inxi baseLngth)) / (sy fs)

fctSftyDerivEqn4 :: Expr
fctSftyDerivEqn4 = (eqlExprSepG sin cos (+)) + 
  (- inxiM1 intShrForce + inxi intShrForce) * (sin (inxi baseAngle)) $= 
  (((eqlExprNSepG cos sin (+)) + 
  (- inxiM1 intShrForce + inxi intShrForce) * (cos (inxi baseAngle)) - 
  (inxi baseHydroForce)) * 
  tan (sy fricAngle) + (sy effCohesion) * (inxi baseLngth)) / (sy fs)

fctSftyDerivEqn5 :: Expr
fctSftyDerivEqn5 = (eqlExprNoKQ sin cos (+)) + 
  (- inxiM1 intShrForce + inxi intShrForce) * (sin (inxi baseAngle)) $= 
  (((eqlExprNNoKQ cos sin (+)) + 
  (- inxiM1 intShrForce + inxi intShrForce) * (cos (inxi baseAngle)) - 
  (inxi baseHydroForce)) * 
  tan (sy fricAngle) + (sy effCohesion) * (inxi baseLngth)) / (sy fs)

fctSftyDerivEqn6 :: Expr
fctSftyDerivEqn6 = (inxi shearFNoIntsl + (- inxiM1 intShrForce + 
  inxi intShrForce) * (sin (inxi baseAngle)) - (- (inxi intNormForce) +
  (inxiM1 intNormForce)) * (cos (inxi baseAngle))) $= (inxi shearRNoIntsl + 
  ((- inxiM1 intShrForce + inxi intShrForce) * (cos (inxi baseAngle)) + 
  (- (inxi intNormForce) + (inxiM1 intNormForce)) * (sin (inxi baseAngle))) * 
  tan (sy fricAngle)) / (sy fs)

fctSftyDerivEqn7 :: Expr
fctSftyDerivEqn7 = (inxi shearFNoIntsl + (- sy normToShear * inxiM1 scalFunc *  
  inxiM1 intNormForce + sy normToShear * inxi scalFunc * inxi intNormForce) * (sin (inxi baseAngle)) - (- (inxi intNormForce) + (inxiM1 intNormForce)) * (cos (inxi baseAngle))) $= (inxi shearRNoIntsl + ((- sy normToShear * 
  inxiM1 scalFunc * inxiM1 intNormForce + sy normToShear * inxi scalFunc * 
  inxi intNormForce) * (cos (inxi baseAngle)) + (- (inxi intNormForce) + (inxiM1 intNormForce)) * (sin (inxi baseAngle))) * tan (sy fricAngle)) / 
  (sy fs)

fctSftyDerivEqn8 :: Expr
fctSftyDerivEqn8 = (inxi intNormForce * ((sy normToShear * inxi scalFunc * 
  (cos (inxi baseAngle)) - (sin (inxi baseAngle))) * tan (sy fricAngle) - 
  (sy normToShear * inxi scalFunc * (sin (inxi baseAngle)) + 
  (cos (inxi baseAngle))) * (sy fs))) $= (inxiM1 intNormForce * 
  ((sy normToShear * inxiM1 scalFunc * (cos (inxi baseAngle)) - 
  (sin (inxi baseAngle))) * tan (sy fricAngle) - (sy normToShear * 
  inxiM1 scalFunc * (sin (inxi baseAngle)) + (cos (inxi baseAngle))) * 
  (sy fs)) + (sy fs) * inxi shearFNoIntsl - inxi shearRNoIntsl)

fctSftyDerivEqn9 :: Expr
fctSftyDerivEqn9 = (inxi intNormForce * inxi shrResC) $= (inxiM1 mobShrC * 
  inxiM1 intNormForce * inxiM1 shrResC + sy fs * inxi shearFNoIntsl -
  inxi shearRNoIntsl)

fctSftyDerivEqn10a :: Expr
fctSftyDerivEqn10a = sliceExpr 1

fctSftyDerivEqn10b :: Expr
fctSftyDerivEqn10b = sliceExpr 2

fctSftyDerivEqn10c :: Expr
fctSftyDerivEqn10c = sliceExpr 3

fctSftyDerivEqn10d :: Expr
fctSftyDerivEqn10d = idx (sy intNormForce) (sy numbSlices - int 2) * 
  idx (sy shrResC) (sy numbSlices - int 2) $= idx (sy mobShrC) (sy numbSlices - int 3) * idx (sy intNormForce) (sy numbSlices - int 3) * 
  idx (sy shrResC) (sy numbSlices - int 3) + sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices - int 2) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 2)

fctSftyDerivEqn10e :: Expr
fctSftyDerivEqn10e = idx (sy intNormForce) (sy numbSlices - int 1) * 
  idx (sy shrResC) (sy numbSlices - int 1) $= idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy intNormForce) (sy numbSlices - int 2) * 
  idx (sy shrResC) (sy numbSlices - int 2) + sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices - int 1) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 1)

fctSftyDerivEqn10f :: Expr
fctSftyDerivEqn10f = idx (sy intNormForce) (sy numbSlices) * 
  idx (sy shrResC) (sy numbSlices) $= idx (sy mobShrC) (sy numbSlices - int 1) * idx (sy intNormForce) (sy numbSlices - int 1) * 
  idx (sy shrResC) (sy numbSlices - int 1) + sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices) - 
  idx (sy shearRNoIntsl) (sy numbSlices)

fctSftyDerivEqn11 :: Expr
fctSftyDerivEqn11 = indx1 intNormForce * indx1 shrResC $= 
  sy fs * indx1 shearFNoIntsl - indx1 shearRNoIntsl

fctSftyDerivEqn12 :: Expr
fctSftyDerivEqn12 = - (sy fs * indxn shearFNoIntsl - indxn shearRNoIntsl) /
  idx (sy mobShrC) (sy numbSlices - int 1) $= 
  idx (sy intNormForce) (sy numbSlices - int 1) * 
  idx (sy shrResC) (sy numbSlices - int 1)

fctSftyDerivEqn13 :: Expr
fctSftyDerivEqn13 = idx (sy intNormForce) 2 * idx (sy shrResC) 2 $= 
  idx (sy mobShrC) 1 * (sy fs * idx (sy shearFNoIntsl) 1 -
  idx (sy shearRNoIntsl) 1) + sy fs * idx (sy shearFNoIntsl) 2 - 
  idx (sy shearRNoIntsl) 2

fctSftyDerivEqn14 :: Expr
fctSftyDerivEqn14 = idx (sy intNormForce) 3 * idx (sy shrResC) 3 $= 
  idx (sy mobShrC) 2 * (idx (sy mobShrC) 1 * (sy fs * idx (sy shearFNoIntsl) 1 -
  idx (sy shearRNoIntsl) 1) + sy fs * idx (sy shearFNoIntsl) 2 - 
  idx (sy shearRNoIntsl) 2) + sy fs * idx (sy shearFNoIntsl) 3 - 
  idx (sy shearRNoIntsl) 3

-- Need to add ellipses where appropriate
fctSftyDerivEqn15 :: Expr
fctSftyDerivEqn15 = idx (sy intNormForce) (sy numbSlices - int 1) * 
  idx (sy shrResC) (sy numbSlices - int 1) $= idx (sy mobShrC) (sy numbSlices - int 2) * (idx (sy mobShrC) (sy numbSlices - int 3) * ((idx (sy mobShrC) 1 * (sy fs * idx (sy shearFNoIntsl) 1 - idx (sy shearRNoIntsl) 1) + sy fs * 
  idx (sy shearFNoIntsl) 2 - idx (sy shearRNoIntsl) 2)) + sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices - int 2) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 2)) + sy fs *
  idx (sy shearFNoIntsl) (sy numbSlices - int 1) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 1)

-- Ellipses needed here too
fctSftyDerivEqn16 :: Expr
fctSftyDerivEqn16 = - (sy fs * indxn shearFNoIntsl - indxn shearRNoIntsl) /
  idx (sy mobShrC) (sy numbSlices - int 1) $= idx (sy mobShrC) (sy numbSlices - 
  int 2) * (idx (sy mobShrC) (sy numbSlices - int 3) * ((idx (sy mobShrC) 1 * 
  (sy fs * idx (sy shearFNoIntsl) 1 - idx (sy shearRNoIntsl) 1) + sy fs * 
  idx (sy shearFNoIntsl) 2 - idx (sy shearRNoIntsl) 2)) + sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices - int 2) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 2)) + sy fs *
  idx (sy shearFNoIntsl) (sy numbSlices - int 1) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 1)

-- Ellipses needed here too
fctSftyDerivEqn17 :: Expr
fctSftyDerivEqn17 = - (sy fs * indxn shearFNoIntsl - indxn shearRNoIntsl) $=
  idx (sy mobShrC) (sy numbSlices - int 1) * idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 1 * (sy fs * indx1 shearFNoIntsl - 
  indx1 shearRNoIntsl) + idx (sy mobShrC) (sy numbSlices - int 1) * idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 2 * (sy fs * 
  idx (sy shearFNoIntsl) 2 - idx (sy shearRNoIntsl) 2) +
  idx (sy mobShrC) (sy numbSlices - int 1) * (sy fs * 
  idx (sy shearFNoIntsl) (sy numbSlices - int 1) - 
  idx (sy shearRNoIntsl) (sy numbSlices - int 1))

-- Ellipses needed here too
fctSftyDerivEqn18 :: Expr
fctSftyDerivEqn18 = sy fs * (idx (sy mobShrC) (sy numbSlices - int 1) * 
  idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 1 * 
  idx (sy shearFNoIntsl) 1 + idx (sy mobShrC) (sy numbSlices - int 1) * 
  idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 2 *
  idx (sy shearFNoIntsl) 2 + idx (sy mobShrC) (sy numbSlices - int 1) *
  idx (sy shearFNoIntsl) (sy numbSlices - int 1) + indxn shearFNoIntsl) $=
  idx (sy mobShrC) (sy numbSlices - int 1) * 
  idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 1 * 
  idx (sy shearRNoIntsl) 1 + idx (sy mobShrC) (sy numbSlices - int 1) * 
  idx (sy mobShrC) (sy numbSlices - int 2) * idx (sy mobShrC) 2 *
  idx (sy shearRNoIntsl) 2 + idx (sy mobShrC) (sy numbSlices - int 1) *
  idx (sy shearRNoIntsl) (sy numbSlices - int 1) + indxn shearRNoIntsl

------------------------------------------------------------------------

nrmShrFor :: InstanceModel
nrmShrFor = im'' nrmShrFor_rc [qw slopeDist, qw slopeHght, qw waterHght, 
  qw waterWeight, qw slipDist, qw slipHght, qw constF]
  [] (qw normToShear) [] [chen2005] nrmShrDeriv "nrmShrFor" [nrmShrF_desc]

nrmShrFor_rc :: RelationConcept
nrmShrFor_rc = makeRC "nrmShrFor_rc" (nounPhraseSP "normal and shear force proportionality constant")
  nrmShrF_desc nrmShrF_rel -- nrmShrForL

nrmShrF_rel :: Relation
nrmShrF_rel = sy normToShear $= sum1toN (inxi nrmShearNum) / sum1toN (inxi nrmShearDen)

nrmShrF_desc :: Sentence
nrmShrF_desc = foldlSent [ch nrmShearNum, S "is defined in", 
  makeRef2S nrmShrForNum `sAnd` ch nrmShearDen, S "is defined in",
  makeRef2S nrmShrForDen]

nrmShrDeriv :: Derivation
nrmShrDeriv = (weave [nrmShrDerivationSentences, map E nrmShrDerivEqns]) ++ nrmShrDerivSentence5

nrmShrDerivSentence1 :: [Sentence]
nrmShrDerivSentence1 = [S "From the", phrase momentEqlGD `sOf`
  makeRef2S momentEqlGD, S "with the primary", phrase assumption, 
  S "for the Morgenstern-Price method of", makeRef2S assumpINSFL `sAnd`
  S "associated", phrase definition, makeRef2S normShrRGD `sC` eqN 14, 
  S "can be derived"]

nrmShrDerivSentence2 :: [Sentence]
nrmShrDerivSentence2 = [S "Rearranging the", phrase equation, S "in terms of",
  ch normToShear, S "leads to", eqN 15]

nrmShrDerivSentence3 :: [Sentence]
nrmShrDerivSentence3 = [S "This", phrase equation, S "can be simplified by",
  S "applying", plural assumption, makeRef2S assumpSF `sAnd` 
  makeRef2S assumpSL `sC` S "which state that the seismic" `sAnd` 
  plural surfLoad `sC` S "respectively" `sC` S "are zero"]

nrmShrDerivSentence4 :: [Sentence]
nrmShrDerivSentence4 = [S "Taking the summation of all", plural slice `sC`
  S "and applying", makeRef2S assumpES, S "to set", 
  E (idx (sy intNormForce) 0) `sC` E (indxn intNormForce) `sC`
  E (idx (sy watrForce) 0) `sC` S "and", E (indxn watrForce), 
  S "equal to zero" `sC` S "a general", phrase equation, S "for the", 
  getTandS normToShear, S "is developed in", eqN 16 `sC` S "which combines", makeRef2S nrmShrFor `sC` makeRef2S nrmShrForNum `sC` S "and",
  makeRef2S nrmShrForDen]

nrmShrDerivSentence5 :: [Sentence]
nrmShrDerivSentence5 = [eqN 16 +:+ S "for" +:+ ch normToShear +:+
  S "is a function of the unknown" +:+ getTandS intNormForce +:+
  sParen (makeRef2S intsliceFs) +:+ S "which itself depends on the unknown" +:+ 
  getTandS fs +:+. sParen (makeRef2S fctSfty)]

nrmShrDerivationSentences :: [Sentence]
nrmShrDerivationSentences = map foldlSentCol [nrmShrDerivSentence1, 
  nrmShrDerivSentence2, nrmShrDerivSentence3, nrmShrDerivSentence4]

nrmShrDerivEqns :: [Expr]
nrmShrDerivEqns = [nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3, 
  nrmShrDerivEqn4]

nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3, nrmShrDerivEqn4 :: Expr
nrmShrDerivEqn1 = 0 $=
  momExpr (\ x y -> x - (sy normToShear * (inxi baseWthX / 2) * 
  (inxi intNormForce * inxi scalFunc + inxiM1 intNormForce *
  inxiM1 scalFunc)) + y)

nrmShrDerivEqn2 = sy normToShear $= momExpr (+)
  / ((inxi baseWthX / 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc))

nrmShrDerivEqn3 = sy normToShear $= momExprNoKQ (-)
  / ((inxi baseWthX / 2) * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc))

nrmShrDerivEqn4 = inxi normToShear $= sum1toN
  (inxi baseWthX * (sy nrmForceSum + sy watForceSum) * tan(inxi baseAngle) +
  inxi midpntHght * (negate (2 * inxi surfHydroForce * sin(inxi surfAngle)))) 
  / sum1toN
  (inxi baseWthX * (inxi intNormForce * inxi scalFunc +
  inxiM1 intNormForce * inxiM1 scalFunc))

---------------------------------------------------------------------

nrmShrForNum :: InstanceModel
nrmShrForNum = im'' nrmShrForNum_rc [qw slopeDist, qw slopeHght, qw waterHght, 
  qw waterWeight, qw slipDist, qw slipHght]
  [] (qw nrmShearNum) [] [chen2005] nrmShrFNum_deriv "nrmShrForNum" 
  [nrmShrFNum_desc]

nrmShrForNum_rc :: RelationConcept
nrmShrForNum_rc = makeRC "nrmShrForNum_rc" (nounPhraseSP "normal and shear force proportionality constant numerator")
  nrmShrFNum_desc nrmShrFNum_rel 

nrmShrFNum_rel :: Relation
nrmShrFNum_rel = inxi nrmShearNum $= case_ [case1,case2,case3]
  where case1 = ((indx1 baseWthX)*((indx1 intNormForce)+(indx1 watrForce)) *
          tan (indx1 baseAngle), sy index $= 1)
        case2 = ((inxi baseWthX)*
          (sy nrmForceSum + sy watForceSum)
           * tan (inxi baseAngle) + (sy midpntHght) * (negate
          2 * inxi surfHydroForce * sin (inxi surfAngle)),
          2 $<= sy index $<= ((sy numbSlices) - 1))
        case3 = ((indxn baseWthX)*(idx (sy intNormForce)
          (sy numbSlices -1) + idx (sy watrForce)
          (sy numbSlices - 1)) * tan (idx (sy baseAngle)
          (sy numbSlices - 1)), sy index $= (sy numbSlices))

nrmShrFNum_deriv :: Derivation
nrmShrFNum_deriv = [S "See" +:+ makeRef2S nrmShrFor +:+ 
  S "for the derivation of" +:+. ch nrmShearNum]

nrmShrFNum_desc :: Sentence
nrmShrFNum_desc = foldlSent [ch baseWthX, S "is defined in", 
  makeRef2S lengthB `sC` ch watrForce, S "is defined in", 
  makeRef2S intersliceWtrF `sC` ch baseAngle, S "is defined in", 
  makeRef2S angleA `sC` ch midpntHght, S "is defined in", 
  makeRef2S slcHeight `sC` ch surfHydroForce, S "is defined in",
  makeRef2S surfWtrF `sC` S "and", ch surfAngle, S "is defined in", 
  makeRef2S angleB]

---------------------------------------------------------------------------

nrmShrForDen :: InstanceModel
nrmShrForDen = im'' nrmShrForDen_rc [qw slipDist, qw constF]
  [] (qw nrmShearDen) [] [chen2005] nrmShrFDen_deriv "nrmShrForDen" 
  [nrmShrFDen_desc]

nrmShrForDen_rc :: RelationConcept
nrmShrForDen_rc = makeRC "nrmShrForDen_rc" (nounPhraseSP "normal and shear force proportionality constant denominator")
  nrmShrFDen_desc nrmShrFDen_rel 

nrmShrFDen_rel :: Relation
nrmShrFDen_rel = inxi nrmShearDen $= case_ [
  (indx1 baseWthX * indx1 scalFunc * indx1 intNormForce, sy index $= 1),
  (inxi baseWthX * (inxi scalFunc * inxi intNormForce +
    inxiM1 scalFunc  * inxiM1 intNormForce),
    2 $<= sy index $<= (sy numbSlices - 1)),
  (indxn baseWthX * idx (sy intNormForce) (sy numbSlices - 1) *
    idx (sy scalFunc) (sy numbSlices - 1), sy index $= 1)
  ]

nrmShrFDen_deriv :: Derivation
nrmShrFDen_deriv = [S "See" +:+ makeRef2S nrmShrFor +:+ 
  S "for the derivation of" +:+. ch nrmShearDen]

nrmShrFDen_desc :: Sentence
nrmShrFDen_desc = foldlSent [ch baseWthX, S "is defined in", 
  makeRef2S lengthB `sAnd` ch scalFunc, S "is defined in",
  makeRef2S ratioVariation]

--------------------------------------------------------------------------

intsliceFs :: InstanceModel
intsliceFs = im'' intsliceFs_rc [qw slopeDist, qw slopeHght, qw waterHght, qw effCohesion, qw fricAngle, qw dryWeight, qw satWeight, qw waterWeight, qw slipDist, qw slipHght, qw constF]
  [] (qw intNormForce) [] [chen2005] intrSlcDeriv "intsliceFs" [sliceFs_desc]

intsliceFs_rc :: RelationConcept
intsliceFs_rc = makeRC "intsliceFs_rc" (nounPhraseSP "interslice normal forces")
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
sliceFs_desc = foldlSent [ch shearFNoIntsl, S "is defined in", 
  makeRef2S mobShearWOGD `sC` ch shearRNoIntsl, S "is defined in",
  makeRef2S resShearWOGD `sC` ch shrResC, S "is defined in",
  makeRef2S convertFunc1 `sC` S "and", ch mobShrC, S "is defined in",
  makeRef2S convertFunc2]

intrSlcDeriv :: Derivation
intrSlcDeriv = weave [intrSlcDerivationSentences, map E intrSlcDerivEqns] ++
  intrSlcDerivSentence3

intrSlcDerivSentence1 :: [Sentence]
intrSlcDerivSentence1 = [S "This derivation is identical to the derivation for",
  makeRef2S fctSfty, S "up until", eqN 3, S "shown again below"] 

intrSlcDerivSentence2 :: [Sentence]
intrSlcDerivSentence2 = [S "A simple rearrangement of", eqN 3, S "leads to",
  eqN 17 `sC` S "also seen in", makeRef2S intsliceFs]

intrSlcDerivSentence3 :: [Sentence]
intrSlcDerivSentence3 = [S "The cases shown in" +:+ makeRef2S intsliceFs +:+
  S "for when" +:+ E (sy index $= 0) `sC` E (sy index $= 1) `sC` S "or" +:+
  E (sy index $= sy numbSlices) +:+ S "are derived by applying" +:+
  makeRef2S assumpES `sC` S "which says that" +:+
  E (idx (sy intNormForce) 0) `sAnd` E (indxn intNormForce) +:+ 
  S "are zero" `sC` S "to" +:+. eqN 17 +:+ ch intNormForce +:+
  S "depends on the unknowns" +:+ ch fs +:+ sParen (makeRef2S fctSfty) `sAnd`
  ch normToShear +:+. sParen (makeRef2S nrmShrFor)]

intrSlcDerivationSentences :: [Sentence]
intrSlcDerivationSentences = map foldlSentCol [intrSlcDerivSentence1, 
  intrSlcDerivSentence2]

intrSlcDerivEqns :: [Expr]
intrSlcDerivEqns = [fctSftyDerivEqn9, intrSlcDerivEqn]

intrSlcDerivEqn :: Expr
intrSlcDerivEqn = (inxi intNormForce) $= 
  (inxiM1 mobShrC * inxiM1 intNormForce +
  sy fs * inxi shearFNoIntsl - inxi shearRNoIntsl) / inxi shrResC

--------------------------------------------------------------------------
crtSlpId :: InstanceModel
crtSlpId = im' crtSlpId_rc [qw slopeDist, qw slopeHght, qw waterDist, 
  qw waterHght, qw effCohesion, qw fricAngle, qw dryWeight, qw satWeight,
  qw waterWeight, qw constF] [] (qw fs_min) [] [li2010] "crtSlpId" 
  [crtSlpId_desc]

crtSlpId_rc :: RelationConcept
crtSlpId_rc = makeRC "crtSlpId_rc" (nounPhraseSP "critical slip surface identification")
  crtSlpId_desc crtSlpId_rel -- crtSlpIdL

-- FIXME: horrible hack. This is short an argument... that was never defined!
-- FIXME: critCoords should also be an output
crtSlpId_rel :: Relation
crtSlpId_rel = (sy fs_min) $= (apply (sy minFunction) [sy slopeDist, 
  sy slopeHght, sy waterDist, sy waterHght, sy effCohesion, sy fricAngle, 
  sy dryWeight, sy satWeight, sy waterWeight, sy constF])

-- FIXME: The constraints described here should be replaced with formal constraints on the input variables once that is possible
crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "The", phrase minFunction, S "must enforce the",
  plural constraint, S "on the", phrase crtSlpSrf, S "expressed in" +:+.
  (makeRef2S assumpSSC `sAnd` makeRef2S data_constraint_Table3), 
  S "The sizes of", ch waterDist `sAnd` ch waterHght +:+. 
  S "must be equal and not 1", S "The", S "sizes of", ch slopeDist `sAnd` 
  ch slopeHght +:+. (S "must be equal" `sAnd` S "at least 2"),
  S "The", phrase first `sAnd` S "last", ch waterDist, 
  plural value, S "must be equal to the", phrase first `sAnd` S "last",
  ch slopeDist +:+. plural value, ch waterDist `sAnd` ch slopeDist, 
  plural value +:+. S "must be monotonically increasing", ch xMaxExtSlip `sC`
  ch xMaxEtrSlip `sC` ch xMinExtSlip `sC` S "and", ch xMinEtrSlip, S "must be",
  S "between or equal to the minimum and maximum", ch slopeDist +:+. 
  plural value, ch yMaxSlip, S "cannot be below the minimum", ch slopeHght +:+.
  phrase value, ch yMinSlip, S "cannot be above the maximum", ch slopeHght +:+.
  phrase value, S "All", ch xi, plural value `sOf` ch critCoords, S "must be",
  S "between" +:+. (ch xMinEtrSlip `sAnd` ch xMaxExtSlip), S "All", ch yi, 
  plural value `sOf` ch critCoords, S "must not be below" +:+. ch yMinSlip, 
  S "For any given vertex in", ch critCoords, S "the", ch yi, phrase value, 
  S "must not exceed the", ch slopeHght, phrase value, S "corresponding to the",
  S "same", ch xi +:+. phrase value, S "The", phrase first `sAnd` S "last",
  S "vertices in", ch critCoords, S "must each be equal to one of the vertices",
  S "formed by" +:+. (ch slopeDist `sAnd` ch slopeHght), S "The slope between",
  S "consecutive vertices must be always increasing as", ch xi +:+.
  S "increases", S "The internal", phrase angle, S "between consecutive",
  S "vertices in", ch critCoords +:+ S "must not be below 110 degrees"]

-----------
-- Intro --
-----------

instModIntro1, instModIntro2 :: Sentence

instModIntro1 = foldlSent [S "The", plural goal,
  S "are met by the simultaneous", phrase solution, S "of", 
  makeRef2S fctSfty `sC` makeRef2S nrmShrFor `sC`
  S "and" +:+. makeRef2S intsliceFs, S "The", phrase goal, S "is also", 
  S "contributed to by" +:+ makeRef2S crtSlpId]

instModIntro2 = foldlSent [S "The", titleize morPrice,
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, at_start analysis, S "is performed by",
  S "breaking the assumed", phrase slpSrf,
  S "into a series of vertical", plural slice, S "of" +:+. phrase mass,
  S "Static equilibrium analysis is performed, using two", phrase force,
  plural equation `sAnd` S "one moment", phrase equation, S "as in" +:+. makeRef2S equilibrium,
  S "The", phrase problem, S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  makeRef2S mcShrStrgth, S "so the", phrase assumption, 
  makeRef2S normShrRGD `sAnd` S "corresponding equation", 
  makeRef2S normShrRGD +:+. S "are used",
  S "The", phrase force, S "equilibrium", plural equation, S "can be modified",
  S "to be expressed only in terms of known", phrase physical, plural value `sC`
  S "as done in", makeRef2S resShearWOGD `sC` makeRef2S mobShearWOGD]