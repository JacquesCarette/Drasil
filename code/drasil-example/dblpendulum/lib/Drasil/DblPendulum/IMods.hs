{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.IMods (iMods, angularAccelIM_1, angularAccelIM_2) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil (foldlSentCol, weave)
import qualified Utils.Drasil.Sentence as S
import Drasil.DblPendulum.Expressions (angularAccelExpr_1, angularAccelExpr_2,
  forceDerivExpr1, forceDerivExpr2,
  cosAngleExpr1, sinAngleExpr1, cosAngleExpr2, sinAngleExpr2)
import Drasil.DblPendulum.Derivations (angularAccelDerivEqns)
import Drasil.DblPendulum.Unitals (pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2,
  massObj_1, massObj_2, angularAccel_1, angularAccel_2, angularVel_1, angularVel_2)
import Drasil.DblPendulum.GenDefs (xForceGD_2, yForceGD_2, xForceGD_1, yForceGD_1, accelXGD_1, accelXGD_2, accelYGD_1, accelYGD_2)

iMods :: [InstanceModel]
iMods = [angularAccelIM_1, angularAccelIM_2]

angularAccelDerivSents :: [Sentence]
angularAccelDerivSents = [angularAccelDerivSent1, EmptyS, angularAccelDerivSent2, EmptyS, angularAccelDerivSent3,
                       angularAccelDerivSent4, EmptyS, angularAccelDerivSent5, angularAccelDerivSent6]

angularAccelDerivSent1, angularAccelDerivSent2, angularAccelDerivSent3,
  angularAccelDerivSent4, angularAccelDerivSent5, angularAccelDerivSent6 :: Sentence
angularAccelDerivSent1 = foldlSentCol [S "By solving equations" +:+ refS xForceGD_2 `S.and_` refS yForceGD_2 
                        `S.for` eS forceDerivExpr1 `S.and_` eS forceDerivExpr2 `S.and_` S "then substituting into eqaution" +:+ 
                        refS xForceGD_1 `S.and_` refS yForceGD_1 +:+ S ", We can get equations 1 and 2"]
angularAccelDerivSent2 = foldlSentCol [S "Multiply the equation 1 by" +:+ 
                       eS cosAngleExpr1 `S.and_` S "the equation 2 by" +:+ eS sinAngleExpr1 `S.and_`
                       S "rearrange to get"]
angularAccelDerivSent3 = S "This leads to the equation 3"
angularAccelDerivSent4 = foldlSentCol[S "Next, multiply equation" +:+ refS xForceGD_2 +:+ S "by" +:+ 
                       eS cosAngleExpr2 `S.and_` S "equation" +:+ refS yForceGD_2 +:+ S "by" +:+ 
                       eS sinAngleExpr2 `S.and_` S "rearrange to get"]
angularAccelDerivSent5 = S "which leads to equation 4"
angularAccelDerivSent6 = foldlSentCol[S "By giving equations" +:+ refS accelXGD_1 `S.and_` refS accelXGD_2 `S.and_` 
                       refS accelYGD_1 `S.and_` refS accelYGD_2 +:+ 
                       S "plus additional two equations, 3 and 4, we can get" +:+ refS angularAccelIM_1 `S.and_`
                       refS angularAccelIM_2 +:+ S "via a computer algebra program"]

angularAccelIM_1 :: InstanceModel
angularAccelIM_1 = imNoRefs angularAccelMK_1
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw angularAccel_1) [UpFrom (Exc, exactDbl 0)]
  Nothing "calOfAngularAcceleration1" []

angularAccelMK_1 :: ModelKind Expr
angularAccelMK_1 = equationalModel "angularAccelerationIM1"
  (nounPhraseSP "calculation of angular acceleration") angularAccelFD_1

angularAccelFD_1 :: QDefinition Expr
angularAccelFD_1 = mkFuncDefByQ angularAccel_1
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularAccelExpr_1

angularAccelIM_2 :: InstanceModel
angularAccelIM_2 = imNoRefs angularAccelMK_2
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw angularAccel_2) [UpFrom (Exc, exactDbl 0)]
  (Just angularAccelDeriv_2) "calOfAngularAcceleration2" [{-Notes-}]

angularAccelMK_2 :: ModelKind Expr
angularAccelMK_2 = equationalModel "angularAccelerationIM2"
  (nounPhraseSP "calculation of angular acceleration") angularAccelFD_2

angularAccelFD_2 :: QDefinition Expr
angularAccelFD_2 = mkFuncDefByQ angularAccel_2
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularAccelExpr_2

angularAccelDeriv_2 :: Derivation
angularAccelDeriv_2 = mkDerivName (phrase pendDisAngle_2) (weave [angularAccelDerivSents, map eS angularAccelDerivEqns])
