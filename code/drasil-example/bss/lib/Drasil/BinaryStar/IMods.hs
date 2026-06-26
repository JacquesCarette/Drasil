module Drasil.BinaryStar.IMods (iMods, accelIM_X1, accelIM_Y1, accelIM_X2, accelIM_Y2) where

import Language.Drasil
import Language.Drasil.Document (refS)
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (condition)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.BinaryStar.Expressions (accelXExpr_1, accelYExpr_1,
  accelXExpr_2, accelYExpr_2)
import Drasil.BinaryStar.Unitals (mass_1, mass_2,
  xPos_1, yPos_1, xPos_2, yPos_2,
  xAccel_1, yAccel_1, xAccel_2, yAccel_2)
import Drasil.BinaryStar.Assumptions (twoBody, isolated, newtonianGravity,
  nonRelativistic, pointMass, constantMass, inertialFrame, planar,
  nonzeroSeparation)
import Drasil.BinaryStar.TMods (centerOfMassTM, velocityTM, accelTM,
  gravLawTM, relPosTM)

iMods :: [InstanceModel]
iMods = [accelIM_X1, accelIM_Y1, accelIM_X2, accelIM_Y2]

-- | Common notes for all IMs describing the derivation chain and assumptions.
imNotes :: DefinedQuantityDict -> [Sentence]
imNotes acc = [foldlSent [ch acc `S.is`
    S "calculated by solving the", short ode,
    S "together with the initial", plural condition +:+. S "",
    S "This model is derived from Newton's second law",
    sParen (refS newtonSL) `sC`
    S "universal gravitation",
    sParen (refS gravLawTM) `sC`
    S "and the definitions of acceleration",
    sParen (refS accelTM) `sC`
    S "velocity",
    sParen (refS velocityTM) `S.and_`
    S "relative position",
    sParen (refS relPosTM) `sC`
    S "under the center-of-mass constraint",
    sParen (refS centerOfMassTM) +:+. S "",
    S "The assumptions are: two-body system",
    sParen (refS twoBody) `sC`
    S "isolated system",
    sParen (refS isolated) `sC`
    S "Newtonian gravitation",
    sParen (refS newtonianGravity) `sC`
    S "non-relativistic mechanics",
    sParen (refS nonRelativistic) `sC`
    S "point masses",
    sParen (refS pointMass) `sC`
    S "constant masses",
    sParen (refS constantMass) `sC`
    S "inertial reference frame",
    sParen (refS inertialFrame) `sC`
    S "planar motion",
    sParen (refS planar) `S.and_`
    S "non-zero separation",
    sParen (refS nonzeroSeparation)]]

---------------------------------------------------------
-- IM1: x-acceleration of star 1
-- ax₁ = -G · m₂ · (x₁ - x₂) / r₁₂³
---------------------------------------------------------
accelIM_X1 :: InstanceModel
accelIM_X1 = imNoRefs accelMK_X1
  [qwC mass_1 $ UpFrom (Exc, exactDbl 0),
   qwC mass_2 $ UpFrom (Exc, exactDbl 0),
   qwUC xPos_1, qwUC yPos_1,
   qwUC xPos_2, qwUC yPos_2]
  xAccel_1 []
  Nothing "accelX1" (imNotes xAccel_1)

accelMK_X1 :: ModelKind Expr
accelMK_X1 = equationalModel "accelIMX1"
  (nounPhraseSP "x-acceleration of the first star") accelFD_X1

accelFD_X1 :: SimpleQDef
accelFD_X1 = mkFuncDefByQ xAccel_1
  [xPos_1, yPos_1, xPos_2, yPos_2] accelXExpr_1

---------------------------------------------------------
-- IM2: y-acceleration of star 1
-- ay₁ = -G · m₂ · (y₁ - y₂) / r₁₂³
---------------------------------------------------------
accelIM_Y1 :: InstanceModel
accelIM_Y1 = imNoRefs accelMK_Y1
  [qwC mass_1 $ UpFrom (Exc, exactDbl 0),
   qwC mass_2 $ UpFrom (Exc, exactDbl 0),
   qwUC xPos_1, qwUC yPos_1,
   qwUC xPos_2, qwUC yPos_2]
  yAccel_1 []
  Nothing "accelY1" (imNotes yAccel_1)

accelMK_Y1 :: ModelKind Expr
accelMK_Y1 = equationalModel "accelIMY1"
  (nounPhraseSP "y-acceleration of the first star") accelFD_Y1

accelFD_Y1 :: SimpleQDef
accelFD_Y1 = mkFuncDefByQ yAccel_1
  [xPos_1, yPos_1, xPos_2, yPos_2] accelYExpr_1

---------------------------------------------------------
-- IM3: x-acceleration of star 2
-- ax₂ = G · m₁ · (x₁ - x₂) / r₁₂³
---------------------------------------------------------
accelIM_X2 :: InstanceModel
accelIM_X2 = imNoRefs accelMK_X2
  [qwC mass_1 $ UpFrom (Exc, exactDbl 0),
   qwC mass_2 $ UpFrom (Exc, exactDbl 0),
   qwUC xPos_1, qwUC yPos_1,
   qwUC xPos_2, qwUC yPos_2]
  xAccel_2 []
  Nothing "accelX2" (imNotes xAccel_2)

accelMK_X2 :: ModelKind Expr
accelMK_X2 = equationalModel "accelIMX2"
  (nounPhraseSP "x-acceleration of the second star") accelFD_X2

accelFD_X2 :: SimpleQDef
accelFD_X2 = mkFuncDefByQ xAccel_2
  [xPos_1, yPos_1, xPos_2, yPos_2] accelXExpr_2

---------------------------------------------------------
-- IM4: y-acceleration of star 2
-- ay₂ = G · m₁ · (y₁ - y₂) / r₁₂³
---------------------------------------------------------
accelIM_Y2 :: InstanceModel
accelIM_Y2 = imNoRefs accelMK_Y2
  [qwC mass_1 $ UpFrom (Exc, exactDbl 0),
   qwC mass_2 $ UpFrom (Exc, exactDbl 0),
   qwUC xPos_1, qwUC yPos_1,
   qwUC xPos_2, qwUC yPos_2]
  yAccel_2 []
  Nothing "accelY2" (imNotes yAccel_2)

accelMK_Y2 :: ModelKind Expr
accelMK_Y2 = equationalModel "accelIMY2"
  (nounPhraseSP "y-acceleration of the second star") accelFD_Y2

accelFD_Y2 :: SimpleQDef
accelFD_Y2 = mkFuncDefByQ yAccel_2
  [xPos_1, yPos_1, xPos_2, yPos_2] accelYExpr_2
