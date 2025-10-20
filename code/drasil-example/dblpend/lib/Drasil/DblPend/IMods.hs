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
import Drasil.DblPend.Expressions (angularAccelExpr_1, angularAccelExpr_2)
import Drasil.DblPend.GenDefs (mvAccelGD_1, mvAccelGD_2, mvForceGD_1, mvForceGD_2)
import Drasil.DblPend.Unitals
    ( angularAccel_1, angularAccel_2
    , angularVel_1, angularVel_2
    , lenRod_1, lenRod_2
    , massObj_1, massObj_2
    , pendDisAngle_1, pendDisAngle_2
    , mvForce_1, mvForce_2
    )

-- Simple substitute expressions for trigonometric and force terms
cosAngleExpr1, cosAngleExpr2, sinAngleExpr1, sinAngleExpr2 :: ModelExpr
forceDerivExpr1, forceDerivExpr2 :: ModelExpr

cosAngleExpr1 = cos (sy pendDisAngle_1)
cosAngleExpr2 = cos (sy pendDisAngle_2)
sinAngleExpr1 = sin (sy pendDisAngle_1)
sinAngleExpr2 = sin (sy pendDisAngle_2)

forceDerivExpr1 = sy mvForce_1
forceDerivExpr2 = sy mvForce_2

-- | List of all Instance Models
iMods :: [InstanceModel]
iMods = [angleIM_1, angleIM_2]

-- | Derivation sentences for the second angle equation
angleDerivSents :: [Sentence]
angleDerivSents = [angleDerivSent1, EmptyS, angleDerivSent2, angleDerivSent3, EmptyS, angleDerivSent4, angleDerivSent5, EmptyS, angleDerivSent6]

angleDerivSent1, angleDerivSent2, angleDerivSent3, angleDerivSent4, angleDerivSent5, angleDerivSent6 :: Sentence
angleDerivSent1 = foldlSentCol
    [ S "By solving multivector force equations" +:+ refS mvForceGD_1 `S.and_` refS mvForceGD_2
    `S.for` eS forceDerivExpr1 `S.and_` eS forceDerivExpr2
    `S.and_` S "then substituting into the Clifford algebra force equations, we can get equations 1 and 2"
    ]

angleDerivSent2 = foldlSentCol
    [ S "Multiply equation 1 by" +:+ eS cosAngleExpr1
    `S.and_` S "equation 2 by" +:+ eS sinAngleExpr1
    `S.and_` S "then rearrange to get the next intermediate equation"
    ]

angleDerivSent3 = S "This leads to the simplified first angular acceleration equation."

angleDerivSent4 = foldlSentCol
    [ S "Next, multiply multivector force equation" +:+ refS mvForceGD_2
    +:+ S "by" +:+ eS cosAngleExpr2 `S.and_`
      S "the corresponding Clifford force equation by" +:+ eS sinAngleExpr2
    `S.and_` S "rearrange to get the second intermediate equation"
    ]

angleDerivSent5 = S "Which leads to the simplified second angular acceleration equation."

angleDerivSent6 = foldlSentCol
    [ S "By substituting multivector acceleration equations" +:+ refS mvAccelGD_1 `S.and_` refS mvAccelGD_2
    `S.and_` S "with the additional Clifford algebra manipulations, we obtain"
    +:+ refS angleIM_1 `S.and_` refS angleIM_2
    ]

-------------------------------------------------
-- Instance Model for first angular acceleration
-------------------------------------------------
angleIM_1 :: InstanceModel
angleIM_1 = imNoRefs angleMK_1
    [ qwC lenRod_1 $ UpFrom (Exc, exactDbl 0)
    , qwC lenRod_2 $ UpFrom (Exc, exactDbl 0)
    , qwC massObj_1 $ UpFrom (Exc, exactDbl 0)
    , qwC massObj_2 $ UpFrom (Exc, exactDbl 0)
    , qwUC pendDisAngle_1
    , qwUC pendDisAngle_2
    ]
    (dqdWr pendDisAngle_1) []
    Nothing "calOfAngle1"
    [ foldlSent [ ch pendDisAngle_1 `S.is`
      S "calculated by solving the" , short ode
      , S "together with initial" , plural condition `S.and_` refS angleIM_2 ] ]

angleMK_1 :: ModelKind Expr
angleMK_1 = equationalModel "angleIM1"
    (nounPhraseSP "calculation of angle of first rod") angleFD_1

angleFD_1 :: SimpleQDef
angleFD_1 = mkFuncDefByQ angularAccel_1
    [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2]
    angularAccelExpr_1

-------------------------------------------------
-- Instance Model for second angular acceleration
-------------------------------------------------
angleIM_2 :: InstanceModel
angleIM_2 = imNoRefs angleMK_2
    [ qwC lenRod_1 $ UpFrom (Exc, exactDbl 0)
    , qwC lenRod_2 $ UpFrom (Exc, exactDbl 0)
    , qwC massObj_1 $ UpFrom (Exc, exactDbl 0)
    , qwC massObj_2 $ UpFrom (Exc, exactDbl 0)
    , qwUC pendDisAngle_1
    , qwUC pendDisAngle_2
    ]
    (dqdWr pendDisAngle_2) []
    (Just angleDeriv_2) "calOfAngle2"
    [ foldlSent [ ch pendDisAngle_2 `S.is`
      S "calculated by solving the" , short ode
      , S "together with initial" , plural condition `S.and_` refS angleIM_1 ] ]

angleMK_2 :: ModelKind Expr
angleMK_2 = equationalModel "angleIM2"
    (nounPhraseSP "calculation of angle of second rod") angleFD_2

angleFD_2 :: SimpleQDef
angleFD_2 = mkFuncDefByQ angularAccel_2
    [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2]
    angularAccelExpr_2

angleDeriv_2 :: Derivation
angleDeriv_2 = mkDerivName (phrase pendDisAngle_2)
    (weave [angleDerivSents, map eS angularAccelDerivEqns])