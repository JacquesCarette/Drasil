{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.IMods (iMods, angleIM_1, angleIM_2) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil (weave)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (condition)
import Data.Drasil.Concepts.Math (ode)

import Drasil.DblPend.Derivations (angularAccelDerivEqns)
import Drasil.DblPend.Expressions (angularAccelExpr_1, angularAccelExpr_2,
  cosAngleExpr1, cosAngleExpr2, forceDerivExpr1, forceDerivExpr2,
  sinAngleExpr1, sinAngleExpr2)
import Drasil.DblPend.GenDefs (accelXGD_1, accelXGD_2, accelYGD_1, accelYGD_2,
  xForceGD_1, xForceGD_2, yForceGD_1, yForceGD_2)
import Drasil.DblPend.Unitals (angularAccel_1, angularAccel_2, angularVel_1,
  angularVel_2, lenRod_1, lenRod_2, massObj_1, massObj_2, pendDisAngle_1,
  pendDisAngle_2)

iMods :: [InstanceModel]
iMods = [angleIM_1, angleIM_2]

angleDerivSents :: [Sentence]
angleDerivSents = [angleDerivSent1, EmptyS, angleDerivSent2, EmptyS, angleDerivSent3,
                    angleDerivSent4, EmptyS, angleDerivSent5, angleDerivSent6]

angleDerivSent1, angleDerivSent2, angleDerivSent3,
  angleDerivSent4, angleDerivSent5, angleDerivSent6 :: Sentence
angleDerivSent1 = foldlSentCol [S "By solving equations" +:+ refS xForceGD_2 `S.and_` refS yForceGD_2 
                    `S.for` eS forceDerivExpr1 `S.and_` eS forceDerivExpr2 `S.and_` S "then substituting into equation" +:+ 
                    refS xForceGD_1 `S.and_` refS yForceGD_1 +:+ S ", We can get equations 1 and 2"]
angleDerivSent2 = foldlSentCol [S "Multiply the equation 1 by" +:+ 
                    eS cosAngleExpr1 `S.and_` S "the equation 2 by" +:+ eS sinAngleExpr1 `S.and_`
                    S "rearrange to get"]
angleDerivSent3 = S "This leads to the equation 3"
angleDerivSent4 = foldlSentCol[S "Next, multiply equation" +:+ refS xForceGD_2 +:+ S "by" +:+ 
                    eS cosAngleExpr2 `S.and_` S "equation" +:+ refS yForceGD_2 +:+ S "by" +:+ 
                    eS sinAngleExpr2 `S.and_` S "rearrange to get"]
angleDerivSent5 = S "which leads to equation 4"
angleDerivSent6 = foldlSentCol[S "By giving equations" +:+ refS accelXGD_1 `S.and_` refS accelXGD_2 `S.and_` 
                    refS accelYGD_1 `S.and_` refS accelYGD_2 +:+ 
                    S "plus additional two equations, 3 and 4, we can get" +:+ refS angleIM_1 `S.and_`
                    refS angleIM_2 +:+ S "via a computer algebra program"]

angleIM_1 :: InstanceModel
angleIM_1 = imNoRefs angleMK_1
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw pendDisAngle_1) []
  Nothing "calOfAngle1" [foldlSent [ch pendDisAngle_1 `S.is`
      S "calculated by solving the", getAcc ode, S "here together with the initial",
      plural condition `S.and_` refS angleIM_2]]

angleMK_1 :: ModelKind Expr
angleMK_1 = equationalModel "angleIM1"
  (nounPhraseSP "calculation of angle of first rod") angleFD_1

angleFD_1 :: SimpleQDef
angleFD_1 = mkFuncDefByQ angularAccel_1
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularAccelExpr_1

angleIM_2 :: InstanceModel
angleIM_2 = imNoRefs angleMK_2
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw pendDisAngle_2) []
  (Just angleDeriv_2) "calOfAngle2" [foldlSent [ch pendDisAngle_2 `S.is`
      S "calculated by solving the", getAcc ode, S "here together with the initial",
      plural condition `S.and_` refS angleIM_1]]

angleMK_2 :: ModelKind Expr
angleMK_2 = equationalModel "angleIM2"
  (nounPhraseSP "calculation of angle of second rod") angleFD_2

angleFD_2 :: SimpleQDef
angleFD_2 = mkFuncDefByQ angularAccel_2
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularAccelExpr_2

angleDeriv_2 :: Derivation
angleDeriv_2 = mkDerivName (phrase pendDisAngle_2) (weave [angleDerivSents, map eS angularAccelDerivEqns])
