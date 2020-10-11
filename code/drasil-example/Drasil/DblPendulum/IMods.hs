module Drasil.DblPendulum.IMods (iMods, angularAccelerationIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
  --imNoDerivNoRefs, )
import Utils.Drasil

-- import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

-- import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation)
import Data.Drasil.Concepts.Physics (pendulum)
-- equation, xAxis)
import Data.Drasil.Concepts.Documentation (component)
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Data.Drasil.Quantities.Physics (angularAccel, gravitationalAccel,
         angularVelocity, tension, acceleration, force)
--   ixVel, iyPos, iyVel, time, xConstAccel, xPos, yConstAccel, yPos)
import Data.Drasil.Quantities.PhysicalProperties (mass)

-- import Drasil.Projectile.Assumptions (accelXZero, accelYGravity, gravAccelValue,
--   launchOrigin, posXDirection, targetXAxis, timeStartZero, yAxisGravity)
-- import Drasil.Projectile.Concepts (projectile, target)
import Drasil.DblPendulum.TMods (accelerationTM)
import Drasil.DblPendulum.Unitals (lenRod, pendAngle)


iMods :: [InstanceModel]
iMods = [angularAccelerationIM]

---
angularAccelerationIM :: InstanceModel
angularAccelerationIM = imNoRefs angularAccelerationRC 
  [qwC lenRod $ UpFrom (Exc, 0)
  ,qwC pendAngle $ Bounded (Exc, 0) (Exc, sy mass / 2)]
  (qw force) [UpFrom (Exc, 0)]
  (Just angularAccelerationDeriv) "calOfAngularAcceleration" [angleConstraintNote]
  

angularAccelerationRC :: RelationConcept
angularAccelerationRC = makeRC "angularAccelerationRC" (nounPhraseSP "calculation of angular acceleration")
  EmptyS $ sy angularAccel $= negate (sy gravitationalAccel / sy lenRod * sin (sy pendAngle)) 

angularAccelerationDeriv :: Derivation
angularAccelerationDeriv = mkDerivName (phrase angularAccel) (weave [angularAccelerationDerivSents, map E angularAccelerationDerivEqns])

angularAccelerationDerivSents :: [Sentence]
angularAccelerationDerivSents = [angularAccelerationDerivSent1, angularAccelerationDerivSent2, angularAccelerationDerivSent3,
                             angularAccelerationDerivSent4, angularAccelerationDerivSent5]

angularAccelerationDerivSent1, angularAccelerationDerivSent2, angularAccelerationDerivSent3,
  angularAccelerationDerivSent4, angularAccelerationDerivSent5 :: Sentence

angularAccelerationDerivSent1 = foldlSentCol [S "Using the Newton's Law" +:+ E (sy force $= sy mass * sy acceleration)
                  `andThe` phrase pendulum, phrase acceleration `sIn` makeRef2S accelerationTM `sC` S "we have" ]
       
 
angularAccelerationDerivSent2 = foldlSentCol [S "Writing the vector components of the above" +:+ phrase equation +:+
                  S "as separate", phrase equation +:+ S "This gives us two similtaneous", phrase equation `sC` 
                  S "first for the", ch unitVect, phrase component `andThe` S "second for the", ch unitVectj, phrase component]

angularAccelerationDerivSent3 = foldlSentCol [S "Doing some algebraic manipulations to eliminate the unknowwn", phrase tension +:+ 
                   S "Multiply the first", phrase equation +:+ S "by" +:+  E (cos (sy pendAngle)) `andThe` S "second by" +:+ E (sin (sy pendAngle))] 

angularAccelerationDerivSent4 = foldlSentCol [S "Use the first" +:+ phrase equation `sC` S "to substitute for" +:+
                  E (sy tension * cos (sy pendAngle) * sin (sy pendAngle)) +:+ S "in the second", phrase equation ]
angularAccelerationDerivSent5 = foldlSentCol [S "With the trig identity" +:+ E (cos (sy pendAngle) + sin (sy pendAngle) $= 1)]


angularAccelerationDerivEqns :: [Expr]
angularAccelerationDerivEqns = [angularAccelerationDerivEqn1, angularAccelerationDerivEqn2, angularAccelerationDerivEqn3,
                                 angularAccelerationDerivEqn4, angularAccelerationDerivEqn5]

angularAccelerationDerivEqn1, angularAccelerationDerivEqn2, angularAccelerationDerivEqn3,
 angularAccelerationDerivEqn4, angularAccelerationDerivEqn5 :: Expr

angularAccelerationDerivEqn1 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                             $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                              + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

angularAccelerationDerivEqn2 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                             $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                              + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

angularAccelerationDerivEqn3 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin ( sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                             $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                              + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

angularAccelerationDerivEqn4 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                              $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                                 + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

angularAccelerationDerivEqn5 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                              $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                               + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

-- landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4 :: Sentence
-- landPosDerivSent1 = foldlSentCol [S "We know that" +:+.
--   foldlList Comma List [eqnWSource (sy ixPos $= 0) launchOrigin,
--   eqnWSource (sy xConstAccel $= 0) accelXZero],
--   S "Substituting these", plural value, S "into the x-direction" `sOf`
--   makeRef2S posVecGD, S "gives us"]
-- landPosDerivSent2 = foldlSentCol [S "To find the", phrase landPos `sC`
--   S "we want to find the", ch xPos, phrase value, sParen (ch landPos),
--   S "at", phrase flightDur, sParen (S "from" +:+ makeRef2S timeIM)]
-- landPosDerivSent3 = foldlSentCol [S "From", makeRef2S speedIX,
--   sParen (S "with" +:+ E (sy iSpeed $= sy launSpeed)), S "we can replace", ch ixVel]
-- landPosDerivSent4 = S "Rearranging this gives us the required" +: phrase equation


-- landPosDerivEqns :: [Expr]
-- landPosDerivEqns = [landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3, landPosExpr]

-- landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: Expr
-- landPosDerivEqn1 = sy xPos $= sy ixVel * sy time
-- landPosDerivEqn2 = sy landPos $= sy ixVel * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
-- landPosDerivEqn3 = sy landPos $= sy launSpeed * cos (sy launAngle) * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

-- ---
-- offsetIM :: InstanceModel
-- offsetIM = imNoDerivNoRefs offsetRC
--   [qwC landPos $ UpFrom (Exc, 0), qwC targPos $ UpFrom (Exc, 0)]
--   (qw offset) [] "offsetIM" [landPosNote, landAndTargPosConsNote]

-- offsetRC :: RelationConcept
-- offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
--   EmptyS $ sy offset $= sy landPos - sy targPos

-- ---
-- messageIM :: InstanceModel
-- messageIM = imNoDerivNoRefs messageRC 
--   [qwC offset $ UpFrom (Exc, negate (sy landPos))
--   ,qwC targPos $ UpFrom (Exc, 0)]
--   (qw message)
--   [] "messageIM" [offsetNote, targPosConsNote, offsetConsNote, tolNote]

-- messageRC :: RelationConcept
-- messageRC = makeRC "messageRC" (nounPhraseSP "output message") 
--   EmptyS $ sy message $= completeCase [case1, case2, case3]
--   where case1 = (Str "The target was hit.",        abs (sy offset / sy targPos) $< sy tol)
--         case2 = (Str "The projectile fell short.", sy offset $< 0)
--         case3 = (Str "The projectile went long.",  sy offset $> 0)

-- Notes

angleConstraintNote :: Sentence
--gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
--   landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
--   timeConsNote, tolNote :: Sentence

angleConstraintNote = S "The" +:+ phrase constraint +:+
     E (0 $< sy pendAngle $< (sy lenRod / 2)) `sIs` S "from"

-- gravitationalAccelConstNote = ch gravitationalAccelConst `sIs`
--   S "defined in" +:+. makeRef2S gravAccelValue

-- landAndTargPosConsNote = S "The" +:+ plural constraint +:+
--   E (sy landPos $> 0) `sAnd` E (sy targPos $> 0) `sAre` S "from" +:+. makeRef2S posXDirection

-- landPosNote = ch landPos `sIs` S "from" +:+. makeRef2S landPosIM

-- landPosConsNote = S "The" +:+ phrase constraint +:+. makeRef2S gravitationalAccel
--   E (sy landPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

-- offsetNote = ch offset `sIs` S "from" +:+. makeRef2S offsetIM

-- offsetConsNote = foldlSent [S "The", phrase constraint, E (sy offset $> negate (sy landPos)) `sIs`
--   S "from the fact that", E (sy landPos $> 0) `sC` S "from", makeRef2S posXDirection]

-- targPosConsNote = S "The" +:+ phrase constraint +:+
--   E (sy targPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

-- timeConsNote = S "The" +:+ phrase constraint +:+
--   E (sy flightDur $> 0) `sIs` S "from" +:+. makeRef2S timeStartZero

-- tolNote = ch tol `sIs` S "defined in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))
