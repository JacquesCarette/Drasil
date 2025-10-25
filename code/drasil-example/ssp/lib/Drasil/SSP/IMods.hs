{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.DocLang.SRS (propCorSol)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis, assumption, constraint,
  definition, first, goal, method_, physical, problem, solution, value)
import Data.Drasil.Concepts.Math (angle, equation, leftSide)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (force)

import Drasil.SSP.Assumptions (assumpSSC, assumpINSFL,
  assumpES, assumpSF, assumpSL)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, eqlExprSepG, eqlExprNSepG,
  eqlExprNoKQ, eqlExprNNoKQ, sliceExpr, momExpr, momExprNoKQ)
import Drasil.SSP.DataDefs (convertFunc1, convertFunc2,
  intersliceWtrF, lengthB, angleA, angleB, slcHeight, ratioVariation)
import Drasil.SSP.GenDefs (normShrRGD, momentEqlGD, normForcEqGD, mobShearWOGD,
  resShearWOGD, bsShrFEqGD, mobShrGD, srfWtrFGD)
import Drasil.SSP.Goals (goals, identifyCritAndFSGS)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, morPrice, slice,
  slpSrf, ssa)
import Drasil.SSP.References (chen2005, li2010, karchewski2012)
import Drasil.SSP.TMods (equilibrium, mcShrStrgth)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX,
  critCoords,
  effCohesion, constF, dryWeight, earthqkLoadFctr, fricAngle, fs, fsMin,
  index, indx1, indxn, intNormForce, intShrForce, inxi, inxiM1, midpntHght,
  minFunction, mobShrC, mobShrI, nrmForceSum, nrmShearNum, normToShear,
  nrmFSubWat, numbSlices, satWeight, scalFunc, shearFNoIntsl, nrmShearDen,
  shearRNoIntsl, shrResC, slipDist, slipHght, slopeDist, slopeHght, sum1toN,
  surfAngle, surfHydroForce, surfLoad, totNrmForce, varblV, watrForce,
  waterDist, waterHght, waterWeight, watForceSum, xi, xMaxExtSlip, xMaxEtrSlip,
  xMinExtSlip, xMinEtrSlip, yi, yMaxSlip, yMinSlip)

---------------------
-- Instance Models --
---------------------

iMods :: [InstanceModel]
iMods = [fctSfty, nrmShrFor, nrmShrForNum, nrmShrForDen, intsliceFs, crtSlpId]

--

fctSfty :: InstanceModel
fctSfty = im (equationalModel' fctSftyQD)
 [qwUC slopeDist, qwUC slopeHght, qwUC waterHght, qwUC effCohesion, qwUC fricAngle,
  qwUC dryWeight, qwUC satWeight, qwUC waterWeight, qwUC slipDist, qwUC slipHght, qwUC constF]
  (dqdWr fs) [] (map dRef [chen2005, karchewski2012])
  (Just fctSftyDeriv) "fctSfty" [fctSftyDesc]

fctSftyQD :: SimpleQDef
fctSftyQD = mkQuantDef' fs factorOfSafety fctSftyExpr

fctSftyExpr :: PExpr
fctSftyExpr = sumOp shearRNoIntsl $/ sumOp shearFNoIntsl
  where prodOp = defprod (eqSymb varblV) (sy index) (sy numbSlices $- int 1)
          (idx (sy mobShrC) (sy varblV))
        sumOp sym = defsum (eqSymb index) (int 1) (sy numbSlices $- int 1)
          (idx (sy sym) (sy index) $* prodOp) $+ idx (sy sym) (sy numbSlices)

fctSftyDesc :: Sentence
fctSftyDesc = foldlList Comma List [shearRNoIntsl `definedIn'''` resShearWOGD,
  mobShrC `definedIn'''` convertFunc2,
  shearFNoIntsl `definedIn'''` mobShearWOGD]

fctSftyDeriv :: Derivation
fctSftyDeriv = mkDerivNoHeader (weave [fctSftyDerivSentences1, map eS fctSftyDerivEqns1] ++
  map eS [fctSftyDerivEqn10b, fctSftyDerivEqn10c] ++ [fctSftyDerivEllipsis] ++
  map eS [fctSftyDerivEqn10d, fctSftyDerivEqn10e, fctSftyDerivEqn10f] ++
  weave [fctSftyDerivSentences2, map eS fctSftyDerivEqns2] ++
  fctSftyDerivSentence20)

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

fctSftyDerivEqns1 :: (ExprC r, LiteralC r) => [r]
fctSftyDerivEqns1 = [fctSftyDerivEqn1, fctSftyDerivEqn2, fctSftyDerivEqn3,
  fctSftyDerivEqn4, fctSftyDerivEqn5, fctSftyDerivEqn6, fctSftyDerivEqn7,
  fctSftyDerivEqn8, fctSftyDerivEqn9, fctSftyDerivEqn10a]

fctSftyDerivEqns2 :: (ExprC r, LiteralC r) => [r]
fctSftyDerivEqns2 = [fctSftyDerivEqn11, fctSftyDerivEqn12, fctSftyDerivEqn13,
  fctSftyDerivEqn14, fctSftyDerivEqn15, fctSftyDerivEqn16, fctSftyDerivEqn17,
  fctSftyDerivEqn18, sy fs $= fctSftyExpr]

fctSftyDerivSentence1 :: [Sentence]
fctSftyDerivSentence1 = [D.toSent (atStartNP (the mobShrI)), definedIn'' bsShrFEqGD,
  S "can be substituted into the definition" `S.of_` phrase mobShrI,
  S "based on the", phrase fs `sC` S "from",
  refS mobShrGD, S "yielding", eqN 1, S "below"]

fctSftyDerivSentence2 :: [Sentence]
fctSftyDerivSentence2 = [S "An expression for the", phrase nrmFSubWat `sC`
  ch nrmFSubWat `sC` S "can be derived by substituting the",
  phrase totNrmForce, S "equilibrium from", refS normForcEqGD,
  S "into the definition" `S.for` phrase nrmFSubWat, S "from" +:+. refS resShearWOGD, S "This results in", eqN 2]

fctSftyDerivSentence3 :: [Sentence]
fctSftyDerivSentence3 = [S "Substituting", eqN 2, S "into", eqN 1, S "gives"]

fctSftyDerivSentence4 :: [Sentence]
fctSftyDerivSentence4 = [S "Since the", phrase intShrForce,
  ch intShrForce `S.and_` phrase intNormForce, ch intNormForce,
  S "are unknown" `sC` S "they are separated from the other terms as follows"]

fctSftyDerivSentence5 :: [Sentence]
fctSftyDerivSentence5 = [S "Applying assumptions", refS assumpSF `S.and_`
  refS assumpSL `sC` S "which state that the",
  D.toSent (phraseNP (earthqkLoadFctr `andThe` surfLoad)) `sC`
  S "respectively" `sC` S "are zero" `sC` S "allows" `S.for` S "further simplification as shown below"]

fctSftyDerivSentence6 :: [Sentence]
fctSftyDerivSentence6 = [S "The definitions of", refS resShearWOGD
  `S.and_` refS mobShearWOGD, S "are present in this equation" `sC` S "and",
  S "thus can be replaced by", eS (inxi shearRNoIntsl) `S.and_`
  eS (inxi shearFNoIntsl) `sC` S "respectively"]

fctSftyDerivSentence7 :: [Sentence]
fctSftyDerivSentence7 = [D.toSent (atStartNP (the intShrForce)), ch intShrForce,
  S "can be expressed in terms" `S.ofThe` phrase intNormForce,
  ch intNormForce, S "using", refS assumpINSFL `S.and_`
  refS normShrRGD `sC` S "resulting in"]

fctSftyDerivSentence8 :: [Sentence]
fctSftyDerivSentence8 = [S "Rearranging yields the following"]

fctSftyDerivSentence9 :: [Sentence]
fctSftyDerivSentence9 = [S "The definitions for" +:+ ch shrResC `S.and_`
  ch mobShrC +:+ S "from" +:+ refS convertFunc1 `S.and_`
  refS convertFunc2 +:+ S "simplify the above to", eqN 3]

fctSftyDerivSentence10 :: [Sentence]
fctSftyDerivSentence10 = [S "Versions of", eqN 3, S "instantiated for slices",
  S "1 to", ch numbSlices, S "are shown below"]

fctSftyDerivEllipsis :: Sentence
fctSftyDerivEllipsis = S "..."

fctSftyDerivSentence11 :: [Sentence]
fctSftyDerivSentence11 = [S "Applying", refS assumpES `sC`
  S "which says that", eS (idx (sy intNormForce) (int 0)) `S.and_`
  eS (indxn intNormForce), S "are zero" `sC` S "results in the following special cases:",eqN 8, S "for the first slice"]

fctSftyDerivSentence12 :: [Sentence]
fctSftyDerivSentence12 = [S "and", eqN 9, S "for the", ch numbSlices :+:
  S "th slice"]

fctSftyDerivSentence13 :: [Sentence]
fctSftyDerivSentence13 = [S "Substituting", eqN 8, S "into", eqN 4, S "yields", eqN 10]

fctSftyDerivSentence14 :: [Sentence]
fctSftyDerivSentence14 = [S "which can be substituted into", eqN 5, S "to get", eqN 11]

fctSftyDerivSentence15 :: [Sentence]
fctSftyDerivSentence15 = [S "and so on until", eqN 12, S "is obtained from", eqN 7]

fctSftyDerivSentence16 :: [Sentence]
fctSftyDerivSentence16 = [eqN 9, S "can then be substituted into the",
  phrase leftSide `S.of_` eqN 12 `sC` S "resulting in"]

fctSftyDerivSentence17 :: [Sentence]
fctSftyDerivSentence17 = [S "This can be rearranged by multiplying both sides",
  S "by", eS (idx (sy mobShrC) (sy numbSlices $- int 1)) `S.and_`
  S "then distributing the multiplication of each", ch mobShrC,
  S "over addition to obtain"]

fctSftyDerivSentence18 :: [Sentence]
fctSftyDerivSentence18 = [S "The multiplication" `S.ofThe` ch mobShrC,
  S "terms can be further distributed over the subtractions, resulting in the",
  S "equation having terms that each either contain an", ch shearRNoIntsl,
  S "or a" +:+. ch shearFNoIntsl, S "The equation can then be rearranged so",
  S "terms containing an", ch shearRNoIntsl, S "are on one side" `S.ofThe` S "equality" `sC`
  S "and terms containing a", ch shearFNoIntsl +:+.
  S "are on the other", S "The multiplication by the", phrase fs,
  S "is common to all" `S.ofThe` ch shearFNoIntsl, S "terms" `sC` S "and thus can be",
  S "factored out" `sC` S "resulting in"]

fctSftyDerivSentence19 :: [Sentence]
fctSftyDerivSentence19 = [S "Isolating the", phrase fs, S "on the left-hand",
  S "side and using compact notation" `S.for` S "the products and sums yields",
  eqN 13 `sC` S "which can also be seen in", refS fctSfty]

fctSftyDerivSentence20 :: [Sentence]
fctSftyDerivSentence20 = [ch fs +:+ S "depends on the unknowns" +:+
  ch normToShear +:+ sParen (refS nrmShrFor) `S.and_` ch intNormForce +:+.
  sParen (refS intsliceFs)]

fctSftyDerivEqn1 :: PExpr
fctSftyDerivEqn1 = --FIXME: pull the right side of this from GD4
  eqlExpr sin cos (\x y -> x $- inxiM1 intShrForce $+ inxi intShrForce $+ y)
  $= (inxi nrmFSubWat $* tan (sy fricAngle) $+ (sy effCohesion $*
  inxi baseLngth)) $/ sy fs

fctSftyDerivEqn2 :: PExpr
fctSftyDerivEqn2 = inxi nrmFSubWat $= eqlExprN cos sin (\x y -> x $-
  inxiM1 intShrForce $+ inxi intShrForce $+ y) $- inxi baseHydroForce

fctSftyDerivEqn3 :: PExpr
fctSftyDerivEqn3 = eqlExpr sin cos (\x y -> x $- inxiM1 intShrForce $+
  inxi intShrForce $+ y) $= ((eqlExprN cos sin (\x y -> x $-
  inxiM1 intShrForce $+ inxi intShrForce $+ y) $- inxi baseHydroForce) $*
  tan (sy fricAngle) $+ (sy effCohesion $* inxi baseLngth)) $/ sy fs

fctSftyDerivEqn4 :: PExpr
fctSftyDerivEqn4 = eqlExprSepG sin cos ($+) $+
  ((neg (inxiM1 intShrForce) $+ inxi intShrForce) $* sin (inxi baseAngle)) $=
  ((eqlExprNSepG cos sin ($+) $+
  (((neg (inxiM1 intShrForce) $+ inxi intShrForce) $* cos (inxi baseAngle)) $-
  inxi baseHydroForce) $*
  tan (sy fricAngle)) $+ (sy effCohesion $* inxi baseLngth)) $/ sy fs

fctSftyDerivEqn5 :: PExpr
fctSftyDerivEqn5 = eqlExprNoKQ sin cos ($+) $+
  ((neg (inxiM1 intShrForce) $+ inxi intShrForce) $* sin (inxi baseAngle)) $=
  ((eqlExprNNoKQ cos sin ($+) $+
  ((neg (inxiM1 intShrForce) $+ inxi intShrForce) $* cos (inxi baseAngle)) $-
  inxi baseHydroForce) $*
  tan (sy fricAngle) $+ (sy effCohesion $* inxi baseLngth)) $/ sy fs

fctSftyDerivEqn6 :: PExpr
fctSftyDerivEqn6 = (inxi shearFNoIntsl $+ ((neg (inxiM1 intShrForce) $+
  inxi intShrForce) $* sin (inxi baseAngle)) $- ((neg (inxi intNormForce) $+
  inxiM1 intNormForce) $* cos (inxi baseAngle))) $= (inxi shearRNoIntsl $+
  (((neg (inxiM1 intShrForce) $+ inxi intShrForce) $* cos (inxi baseAngle) $+
  ((neg (inxi intNormForce) $+ inxiM1 intNormForce) $* sin (inxi baseAngle))) $*
  tan (sy fricAngle))) $/ sy fs

fctSftyDerivEqn7 :: PExpr
fctSftyDerivEqn7 = (inxi shearFNoIntsl $+ ((neg (sy normToShear) $* inxiM1 scalFunc $*
  inxiM1 intNormForce $+ (sy normToShear $* inxi scalFunc $* inxi intNormForce)) $*
  sin (inxi baseAngle)) $- ((neg (inxi intNormForce) $+ inxiM1 intNormForce) $* cos (inxi baseAngle)))
  $= (inxi shearRNoIntsl $+ (((neg (sy normToShear) $*
  inxiM1 scalFunc $* inxiM1 intNormForce $+ (sy normToShear $* inxi scalFunc $*
  inxi intNormForce)) $* cos (inxi baseAngle) $+ ((neg (inxi intNormForce) $+ inxiM1 intNormForce)
  $* sin (inxi baseAngle))) $* tan (sy fricAngle))) $/ sy fs

fctSftyDerivEqn8 :: PExpr
fctSftyDerivEqn8 = (inxi intNormForce $* ((sy normToShear $* inxi scalFunc $*
  cos (inxi baseAngle) $- sin (inxi baseAngle)) $* tan (sy fricAngle) $-
  ((sy normToShear $* inxi scalFunc $* sin (inxi baseAngle) $+
  cos (inxi baseAngle)) $* sy fs))) $= (inxiM1 intNormForce $*
  ((sy normToShear $* inxiM1 scalFunc $* cos (inxi baseAngle) $-
  sin (inxi baseAngle)) $* tan (sy fricAngle) $- ((sy normToShear $*
  inxiM1 scalFunc $* sin (inxi baseAngle) $+ cos (inxi baseAngle)) $*
  sy fs)) $+ (sy fs $* inxi shearFNoIntsl) $- inxi shearRNoIntsl)

fctSftyDerivEqn9 :: PExpr
fctSftyDerivEqn9 = (inxi intNormForce $* inxi shrResC) $= (inxiM1 mobShrC $*
  inxiM1 intNormForce $* inxiM1 shrResC $+ (sy fs $* inxi shearFNoIntsl) $-
  inxi shearRNoIntsl)

fctSftyDerivEqn10a :: PExpr
fctSftyDerivEqn10a = sliceExpr 1

fctSftyDerivEqn10b :: PExpr
fctSftyDerivEqn10b = sliceExpr 2

fctSftyDerivEqn10c :: PExpr
fctSftyDerivEqn10c = sliceExpr 3

fctSftyDerivEqn10d :: PExpr
fctSftyDerivEqn10d = idx (sy intNormForce) (sy numbSlices $- int 2) $*
  idx (sy shrResC) (sy numbSlices $- int 2) $= (idx (sy mobShrC) (sy numbSlices $- int 3) $* idx (sy intNormForce) (sy numbSlices $- int 3) $*
  idx (sy shrResC) (sy numbSlices $- int 3) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2))

fctSftyDerivEqn10e :: PExpr
fctSftyDerivEqn10e = idx (sy intNormForce) (sy numbSlices $- int 1) $*
  idx (sy shrResC) (sy numbSlices $- int 1) $= (idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy intNormForce) (sy numbSlices $- int 2) $*
  idx (sy shrResC) (sy numbSlices $- int 2) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

fctSftyDerivEqn10f :: PExpr
fctSftyDerivEqn10f = idx (sy intNormForce) (sy numbSlices) $*
  idx (sy shrResC) (sy numbSlices) $= (idx (sy mobShrC) (sy numbSlices $- int 1) $* idx (sy intNormForce) (sy numbSlices $- int 1) $*
  idx (sy shrResC) (sy numbSlices $- int 1) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices)) $-
  idx (sy shearRNoIntsl) (sy numbSlices))

fctSftyDerivEqn11 :: PExpr
fctSftyDerivEqn11 = (indx1 intNormForce $* indx1 shrResC) $=
  (sy fs $* indx1 shearFNoIntsl $- indx1 shearRNoIntsl)

fctSftyDerivEqn12 :: PExpr
fctSftyDerivEqn12 = neg ((sy fs $* indxn shearFNoIntsl $- indxn shearRNoIntsl) $/
  idx (sy mobShrC) (sy numbSlices $- int 1)) $=
  (idx (sy intNormForce) (sy numbSlices $- int 1) $*
  idx (sy shrResC) (sy numbSlices $- int 1))

fctSftyDerivEqn13 :: PExpr
fctSftyDerivEqn13 = idx (sy intNormForce) (int 2) $* idx (sy shrResC) (int 2) $=
  (idx (sy mobShrC) (int 1) $* (sy fs $* idx (sy shearFNoIntsl) (int 1) $-
  idx (sy shearRNoIntsl) (int 1)) $+ (sy fs $* idx (sy shearFNoIntsl) (int 2)) $-
  idx (sy shearRNoIntsl) (int 2))

fctSftyDerivEqn14 :: PExpr
fctSftyDerivEqn14 = idx (sy intNormForce) (int 3) $* idx (sy shrResC) (int 3) $=
  (idx (sy mobShrC) (int 2) $* (idx (sy mobShrC) (int 1) $* (sy fs $* idx (sy shearFNoIntsl) (int 1) $-
  idx (sy shearRNoIntsl) (int 1)) $+ (sy fs $* idx (sy shearFNoIntsl) (int 2)) $-
  idx (sy shearRNoIntsl) (int 2)) $+ (sy fs $* idx (sy shearFNoIntsl) (int 3)) $-
  idx (sy shearRNoIntsl) (int 3))

-- Need to ($+) ellipses where appropriate
fctSftyDerivEqn15 :: PExpr
fctSftyDerivEqn15 = idx (sy intNormForce) (sy numbSlices $- int 1) $*
  idx (sy shrResC) (sy numbSlices $- int 1) $= (idx (sy mobShrC) (sy numbSlices $- int 2) $*
  (idx (sy mobShrC) (sy numbSlices $- int 3) $* (idx (sy mobShrC) (int 1) $*
  (sy fs $* idx (sy shearFNoIntsl) (int 1) $- idx (sy shearRNoIntsl) (int 1)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (int 2)) $- idx (sy shearRNoIntsl) (int 2)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

-- Ellipses needed here too
fctSftyDerivEqn16 :: PExpr
fctSftyDerivEqn16 = neg ((sy fs $* indxn shearFNoIntsl $- indxn shearRNoIntsl) $/
  idx (sy mobShrC) (sy numbSlices $- int 1)) $= (idx (sy mobShrC) (sy numbSlices $-
  int 2) $* (idx (sy mobShrC) (sy numbSlices $- int 3) $* (idx (sy mobShrC) (int 1) $*
  (sy fs $* idx (sy shearFNoIntsl) (int 1) $- idx (sy shearRNoIntsl) (int 1)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (int 2)) $- idx (sy shearRNoIntsl) (int 2)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2)) $+ (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

-- Ellipses needed here too
fctSftyDerivEqn17 :: PExpr
fctSftyDerivEqn17 = neg (sy fs $* indxn shearFNoIntsl $- indxn shearRNoIntsl) $=
  (idx (sy mobShrC) (sy numbSlices $- int 1) $* idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 1) $* (sy fs $* indx1 shearFNoIntsl $-
  indx1 shearRNoIntsl) $+ (idx (sy mobShrC) (sy numbSlices $- int 1) $* idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 2) $* (sy fs $*
  idx (sy shearFNoIntsl) (int 2) $- idx (sy shearRNoIntsl) (int 2))) $+
  (idx (sy mobShrC) (sy numbSlices $- int 1) $* (sy fs $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))))

-- Ellipses needed here too
fctSftyDerivEqn18 :: PExpr
fctSftyDerivEqn18 = sy fs $* (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 1) $*
  idx (sy shearFNoIntsl) (int 1) $+ (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 2) $*
  idx (sy shearFNoIntsl) (int 2)) $+ (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $+ indxn shearFNoIntsl) $=
  (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 1) $*
  idx (sy shearRNoIntsl) (int 1) $+ (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy mobShrC) (sy numbSlices $- int 2) $* idx (sy mobShrC) (int 2) $*
  idx (sy shearRNoIntsl) (int 2)) $+ (idx (sy mobShrC) (sy numbSlices $- int 1) $*
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1)) $+ indxn shearRNoIntsl)

------------------------------------------------------------------------

nrmShrFor :: InstanceModel
nrmShrFor = im nrmShrForMK [qwUC slopeDist, qwUC slopeHght, qwUC waterHght,
  qwUC waterWeight, qwUC slipDist, qwUC slipHght, qwUC constF]
  normToShear [] (map dRef [chen2005, karchewski2012])
  (Just nrmShrDeriv) "nrmShrFor" [nrmShrFDesc]

nrmShrForMK :: ModelKind Expr
nrmShrForMK = equationalModel "nrmShrForIM"
  (nounPhraseSP "normal and shear force proportionality constant") nrmShrForQD

nrmShrForQD :: SimpleQDef
nrmShrForQD = mkQuantDef normToShear nrmShrFExpr

nrmShrFExpr :: PExpr
nrmShrFExpr = sum1toN (inxi nrmShearNum) $/ sum1toN (inxi nrmShearDen)

nrmShrFDesc :: Sentence
nrmShrFDesc = nrmShearNum `definedIn'''`
  nrmShrForNum `S.and_` (nrmShearDen `definedIn'''`
  nrmShrForDen !.)

nrmShrDeriv :: Derivation
nrmShrDeriv = mkDerivNoHeader (weave [nrmShrDerivationSentences, map eS nrmShrDerivEqns] ++
  nrmShrDerivSentence5)

nrmShrDerivSentence1 :: [Sentence]
nrmShrDerivSentence1 = [S "From the", phrase momentEqlGD `S.of_`
  refS momentEqlGD, S "with the primary", phrase assumption `S.for`
  S "the Morgenstern-Price method" `S.of_` refS assumpINSFL `S.and_`
  S "associated", phrase definition, refS normShrRGD `sC` eqN 14,
  S "can be derived"]

nrmShrDerivSentence2 :: [Sentence]
nrmShrDerivSentence2 = [S "Rearranging the", phrase equation, S "in terms of",
  ch normToShear, S "leads to", eqN 15]

nrmShrDerivSentence3 :: [Sentence]
nrmShrDerivSentence3 = [S "This", phrase equation, S "can be simplified by",
  S "applying", plural assumption, refS assumpSF `S.and_`
  refS assumpSL `sC` S "which state that the seismic" `S.and_`
  plural surfLoad `sC` S "respectively" `sC` S "are zero"]

nrmShrDerivSentence4 :: [Sentence]
nrmShrDerivSentence4 = [S "Taking the summation of all", plural slice `sC`
  S "and applying", refS assumpES, S "to set",
  eS (idx (sy intNormForce) (int 0)) `sC` eS (indxn intNormForce) `sC`
  eS (idx (sy watrForce) (int 0)) `sC` S "and", eS (indxn watrForce),
  S "equal to zero" `sC` S "a general", phrase equation, S "for the",
  getTandS normToShear, S "is developed in", eqN 16 `sC` S "which combines", refS nrmShrFor `sC` refS nrmShrForNum `sC` S "and",
  refS nrmShrForDen]

nrmShrDerivSentence5 :: [Sentence]
nrmShrDerivSentence5 = [eqN 16 `S.for` ch normToShear +:+
  S "is a function of the unknown" +:+ getTandS intNormForce +:+
  sParen (refS intsliceFs) +:+ S "which itself depends on the unknown" +:+
  getTandS fs +:+. sParen (refS fctSfty)]

nrmShrDerivationSentences :: [Sentence]
nrmShrDerivationSentences = map foldlSentCol [nrmShrDerivSentence1,
  nrmShrDerivSentence2, nrmShrDerivSentence3, nrmShrDerivSentence4]

nrmShrDerivEqns :: (ExprC r, LiteralC r) => [r]
nrmShrDerivEqns = [nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3,
  nrmShrDerivEqn4]

nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3, nrmShrDerivEqn4 :: PExpr
nrmShrDerivEqn1 = exactDbl 0 $=
  momExpr (\ x y -> x $+ (sy normToShear $* half (inxi baseWthX) $*
  (inxi intNormForce $* inxi scalFunc $+ (inxiM1 intNormForce $*
  inxiM1 scalFunc))) $+ y)

nrmShrDerivEqn2 = sy normToShear $= momExpr ($+)
  $/ neg (half (inxi baseWthX) $* (inxi intNormForce $* inxi scalFunc $+
  (inxiM1 intNormForce $* inxiM1 scalFunc)))

nrmShrDerivEqn3 = sy normToShear $= momExprNoKQ ($+)
  $/ neg (half (inxi baseWthX) $* (inxi intNormForce $* inxi scalFunc $+
  (inxiM1 intNormForce $* inxiM1 scalFunc)))

nrmShrDerivEqn4 = sy normToShear $= sum1toN
  (inxi baseWthX $* (sy nrmForceSum $+ sy watForceSum) $* tan (inxi baseAngle) $+
  (inxi midpntHght $* neg (exactDbl 2 $* inxi surfHydroForce $* sin (inxi surfAngle))))
  $/ sum1toN
  (inxi baseWthX $* (inxi intNormForce $* inxi scalFunc $+
  (inxiM1 intNormForce $* inxiM1 scalFunc)))

---------------------------------------------------------------------

nrmShrForNum :: InstanceModel
nrmShrForNum = im (othModel' nrmShrForNumRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterHght,
  qwUC waterWeight, qwUC slipDist, qwUC slipHght]
  (dqdWr nrmShearNum) [] (map dRef [chen2005, karchewski2012])
  (Just nrmShrFNumDeriv) "nrmShrForNum" [nrmShrFNumDesc]

nrmShrForNumRC :: RelationConcept
nrmShrForNumRC = makeRC "nrmShrForNumRC" (nounPhraseSP "normal and shear force proportionality constant numerator")
  nrmShrFNumDesc nrmShrFNumRel

nrmShrFNumRel :: Relation
nrmShrFNumRel = inxi nrmShearNum $= incompleteCase [case1,case2,case3]
  where case1 = (indx1 baseWthX $* (indx1 intNormForce $+ indx1 watrForce) $*
          tan (indx1 baseAngle), sy index $= int 1)
        case2 = ((inxi baseWthX $*
          (sy nrmForceSum $+ sy watForceSum)
           $* tan (inxi baseAngle)) $+ (sy midpntHght $* (neg
          (exactDbl 2) $* inxi surfHydroForce $* sin (inxi surfAngle))),
          int 2 $<= sy index $<= (sy numbSlices $- int 1))
        case3 = (indxn baseWthX $* (idx (sy intNormForce)
          (sy numbSlices $- int 1) $+ idx (sy watrForce)
          (sy numbSlices $- int 1)) $* tan (idx (sy baseAngle)
          (sy numbSlices $- int 1)), sy index $= sy numbSlices)

nrmShrFNumDeriv :: Derivation
nrmShrFNumDeriv = mkDerivNoHeader [foldlSent [S "See", refS nrmShrFor,
  S "for the derivation" `S.of_` ch nrmShearNum]]

nrmShrFNumDesc :: Sentence
nrmShrFNumDesc = (foldlList Comma List [baseWthX `definedIn'''` lengthB,
  watrForce `definedIn'''` intersliceWtrF,
  baseAngle `definedIn'''` angleA,
  midpntHght `definedIn'''` slcHeight,
  surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB] !.)

---------------------------------------------------------------------------

nrmShrForDen :: InstanceModel
nrmShrForDen = im (othModel' nrmShrForDenRC) [qwUC slipDist, qwUC constF]
  (dqdWr nrmShearDen) [] (map dRef [chen2005, karchewski2012])
  (Just nrmShrFDenDeriv) "nrmShrForDen" [nrmShrFDenDesc]

nrmShrForDenRC :: RelationConcept
nrmShrForDenRC = makeRC "nrmShrForDenRC" (nounPhraseSP "normal and shear force proportionality constant denominator")
  nrmShrFDenDesc nrmShrFDenRel

nrmShrFDenRel :: Relation
nrmShrFDenRel = inxi nrmShearDen $= incompleteCase [
  (indx1 baseWthX $* indx1 scalFunc $* indx1 intNormForce, sy index $= int 1),
  (inxi baseWthX $* (inxi scalFunc $* inxi intNormForce $+
    (inxiM1 scalFunc $* inxiM1 intNormForce)),
    int 2 $<= sy index $<= (sy numbSlices $- int 1)),
  (indxn baseWthX $* idx (sy intNormForce) (sy numbSlices $- int 1) $*
    idx (sy scalFunc) (sy numbSlices $- int 1), sy index $= sy numbSlices)
  ]

nrmShrFDenDeriv :: Derivation
nrmShrFDenDeriv = mkDerivNoHeader [foldlSent [S "See", refS nrmShrFor,
  S "for the derivation" `S.of_` ch nrmShearDen]]

nrmShrFDenDesc :: Sentence
nrmShrFDenDesc = baseWthX `definedIn'''`
  lengthB `S.and_` (scalFunc `definedIn'''`
  ratioVariation !.)

--------------------------------------------------------------------------

intsliceFs :: InstanceModel
intsliceFs = im (othModel' intsliceFsRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterHght
  , qwUC effCohesion, qwUC fricAngle, qwUC dryWeight, qwUC satWeight, qwUC waterWeight
  , qwUC slipDist, qwUC slipHght, qwUC constF]
  (dqdWr intNormForce) [] [dRef chen2005] (Just intrSlcDeriv) "intsliceFs" [sliceFsDesc]

intsliceFsRC :: RelationConcept
intsliceFsRC = makeRC "intsliceFsRC" (nounPhraseSP "interslice normal forces")
  sliceFsDesc sliceFsRel -- inslideFxL

sliceFsRel :: Relation
sliceFsRel = inxi intNormForce $= incompleteCase [
  ((sy fs $* indx1 shearFNoIntsl $- indx1 shearRNoIntsl) $/ indx1 shrResC,
    sy index $= int 1),
  ((inxiM1 mobShrC $* inxiM1 intNormForce $+
    (sy fs $* inxi shearFNoIntsl $- inxi shearRNoIntsl)) $/ inxi shrResC,
    int 2 $<= sy index $<= (sy numbSlices $- int 1)),
  (int 0, sy index $= int 0 $|| sy index $= sy numbSlices)]
  -- FIXME: Use index i as part of condition

sliceFsDesc :: Sentence
sliceFsDesc = (foldlList Comma List [shearFNoIntsl `definedIn'''` mobShearWOGD,
  shearRNoIntsl `definedIn'''` resShearWOGD,
  shrResC `definedIn'''` convertFunc1,
  mobShrC `definedIn'''` convertFunc2] !.)

intrSlcDeriv :: Derivation
intrSlcDeriv = mkDerivNoHeader (weave [intrSlcDerivationSentences, map eS intrSlcDerivEqns] ++ intrSlcDerivSentence3)

intrSlcDerivSentence1 :: [Sentence]
intrSlcDerivSentence1 = [S "This derivation" `S.is` S "identical to the derivation for",
  refS fctSfty, S "up until", eqN 3, S "shown again below"]

intrSlcDerivSentence2 :: [Sentence]
intrSlcDerivSentence2 = [S "A simple rearrangement of", eqN 3, S "leads to",
  eqN 17 `sC` S "also seen in", refS intsliceFs]

intrSlcDerivSentence3 :: [Sentence]
intrSlcDerivSentence3 = [S "The cases shown in" +:+ refS intsliceFs +:+
  S "for when" +:+ eS (sy index $= int 0) `sC` eS (sy index $= int 1) `sC` S "or" +:+
  eS (sy index $= sy numbSlices) +:+ S "are derived by applying" +:+
  refS assumpES `sC` S "which says that" +:+
  eS (idx (sy intNormForce) (int 0)) `S.and_` eS (indxn intNormForce) +:+
  S "are zero" `sC` S "to" +:+. eqN 17 +:+ ch intNormForce +:+
  S "depends on the unknowns" +:+ ch fs +:+ sParen (refS fctSfty) `S.and_`
  ch normToShear +:+. sParen (refS nrmShrFor)]

intrSlcDerivationSentences :: [Sentence]
intrSlcDerivationSentences = map foldlSentCol [intrSlcDerivSentence1,
  intrSlcDerivSentence2]

intrSlcDerivEqns :: (ExprC r, LiteralC r) => [r]
intrSlcDerivEqns = [fctSftyDerivEqn9, intrSlcDerivEqn]

intrSlcDerivEqn :: PExpr
intrSlcDerivEqn = inxi intNormForce $=
  (inxiM1 mobShrC $* inxiM1 intNormForce $+
  (sy fs $* inxi shearFNoIntsl) $- inxi shearRNoIntsl) $/ inxi shrResC

--------------------------------------------------------------------------
crtSlpId :: InstanceModel
crtSlpId = imNoDeriv (othModel' crtSlpIdRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterDist,
  qwUC waterHght, qwUC effCohesion, qwUC fricAngle, qwUC dryWeight, qwUC satWeight,
  qwUC waterWeight, qwUC constF] fsMin [] [dRef li2010] "crtSlpId"
  [crtSlpIdDesc]

crtSlpIdRC :: RelationConcept
crtSlpIdRC = makeRC "crtSlpIdIM"
  (nounPhraseSP "critical slip surface identification")
  crtSlpIdDesc
  crtSlpIdRel

-- placeholder expression hack that's better than the horrible hack
crtSlpIdRel :: Relation
crtSlpIdRel = sy slopeDist $= sy slopeHght

  {- equationalModel "crtSlpIdIM"
  (nounPhraseSP "critical slip surface identification") crtSlpIdQD -}

{-
crtSlpIdQD :: SimpleQDef
crtSlpIdQD = mkQuantDef fsMin crtSlpIdExpr

-- FIXME: horrible hack. This is short an argument... that was never defined!
-- FIXME: critCoords should also be an output
crtSlpIdExpr :: Expr
crtSlpIdExpr = apply minFunction [sy slopeDist,
  sy slopeHght, sy waterDist, sy waterHght, sy effCohesion, sy fricAngle,
  sy dryWeight, sy satWeight, sy waterWeight]
-}

-- FIXME: The constraints described here should be replaced with formal constraints on the input variables once that is possible
crtSlpIdDesc :: Sentence
crtSlpIdDesc = foldlSent [D.toSent (atStartNP (the minFunction)), S "must enforce the",
  D.toSent (pluralNP (constraint `onThePS` crtSlpSrf)), S "expressed in" +:+.
  (refS assumpSSC `S.and_` refS (propCorSol [] [])),
  S "The sizes" `S.of_` ch waterDist `S.and_` ch waterHght +:+.
  S "must be equal and not 1", S "The", S "sizes of", ch slopeDist `S.and_`
  ch slopeHght +:+. (S "must be equal" `S.and_` S "at least 2"),
  D.toSent (atStartNP (the first)) `S.and_` S "last", ch waterDist,
  plural value, S "must be equal" `S.toThe` phrase first `S.and_` S "last",
  ch slopeDist +:+. plural value, ch waterDist `S.and_` ch slopeDist,
  plural value +:+. S "must be monotonically increasing", ch xMaxExtSlip `sC`
  ch xMaxEtrSlip `sC` ch xMinExtSlip `sC` S "and", ch xMinEtrSlip, S "must be",
  S "between or equal" `S.toThe` S "minimum and maximum", ch slopeDist +:+.
  plural value, ch yMaxSlip, S "cannot be below the minimum", ch slopeHght +:+.
  phrase value, ch yMinSlip, S "cannot be above the maximum", ch slopeHght +:+.
  phrase value, S "All", ch xi, plural value `S.of_` ch critCoords, S "must be",
  S "between" +:+. (ch xMinEtrSlip `S.and_` ch xMaxExtSlip), S "All", ch yi,
  plural value `S.of_` ch critCoords, S "must not be below" +:+. ch yMinSlip,
  S "For any given vertex in", ch critCoords, S "the", ch yi, phrase value,
  S "must not exceed the", ch slopeHght, phrase value, S "corresponding to the",
  S "same", ch xi +:+. phrase value, D.toSent (atStartNP (the first)) `S.and_` S "last",
  S "vertices in", ch critCoords, S "must each be equal to one" `S.ofThe` S "vertices",
  S "formed by" +:+. (ch slopeDist `S.and_` ch slopeHght), S "The slope between",
  S "consecutive vertices must be always increasing as", ch xi +:+.
  S "increases", S "The internal", phrase angle, S "between consecutive",
  S "vertices in", ch critCoords +:+ S "must not be below 110 degrees"]

-----------
-- Intro --
-----------

instModIntro :: [Sentence]
instModIntro = [instModIntro1, instModIntro2]

instModIntro1, instModIntro2 :: Sentence

instModIntro1 = foldlSent [D.toSent (atStartNP' (the goal)), foldlList Comma List
  (map refS goals) `S.are` S "met by the simultaneous" +:+. (phrase solution `S.of_`
  foldlList Comma List (map refS [fctSfty, nrmShrFor, intsliceFs])), D.toSent $ atStartNP (the goal),
  refS identifyCritAndFSGS `S.is` S "also contributed to by",
  refS crtSlpId]

instModIntro2 = foldlSent [D.toSent (titleizeNP (the morPrice)),
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, atStart analysis, S "is performed by",
  S "breaking the assumed", phrase slpSrf,
  S "into a series of vertical", plural slice, S "of" +:+. phrase mass,
  S "Static equilibrium analysis is performed, using two", phrase force,
  plural equation `S.and_` S "one moment", phrase equation, S "as in" +:+. refS equilibrium,
  D.toSent (atStartNP (the problem)), S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  refS mcShrStrgth, S "so the", phrase assumption,
  refS normShrRGD `S.and_` S "corresponding equation",
  refS normShrRGD +:+. S "are used",
  D.toSent (atStartNP (the force)), S "equilibrium", plural equation, S "can be modified",
  S "to be expressed only in terms of known", phrase physical, plural value `sC`
  S "as done in", refS resShearWOGD `S.and_` refS mobShearWOGD]
