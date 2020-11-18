module Drasil.DblPendulum.IMods (iMods, angularAccelerationIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
  --imNoDerivNoRefs, )
import Utils.Drasil
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Data.Drasil.Quantities.Physics (angularAccel, gravitationalAccel,
         angularVelocity, tension, acceleration, force, displacement)
import Data.Drasil.Theories.Physics (newtonSL)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.DblPendulum.Unitals (lenRod, pendAngle)
import Data.Drasil.Concepts.Math (constraint)


iMods :: [InstanceModel]
iMods = [angularAccelerationIM]

---
angularAccelerationIM :: InstanceModel
angularAccelerationIM = imNoRefs angularAccelerationRC 
  [qwC lenRod $ UpFrom (Exc, 0)
  ,qwC pendAngle $ UpFrom (Exc, 0)]
  (qw angularAccel) [UpFrom (Exc, 0)]
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

angularAccelerationDerivSent1 = foldlSentCol [S "Using the Newton's Law" `sIn` makeRef2S newtonSL `sC`
                                 S "we have" ]
       
 
angularAccelerationDerivSent2 = foldlSentCol [S "Where", ch force +:+ S "denotes the", phrase force `sC`
                                    ch mass +:+ S "denotes the", phrase mass `sAnd` ch acceleration +:+ 
                                    S "denotes the", phrase acceleration +:+  S "Therefore"]
                 

angularAccelerationDerivSent3 = foldlSentCol [S "Then we have" ] 

angularAccelerationDerivSent4 = foldlSent [S "when the rod makes an" +:+ ch pendAngle +:+ S "with the vertical" `sC` phrase displacement
                                              `ofThe` phrase mass `sIs` S "given by" ]

angularAccelerationDerivSent5 = foldlSentCol [S "With the trig identity" +:+ E (cos (sy pendAngle) + sin (sy pendAngle) $= 1)]


angularAccelerationDerivEqns :: [Expr]
angularAccelerationDerivEqns = [angularAccelerationDerivEqn1, angularAccelerationDerivEqn2, angularAccelerationDerivEqn3,
                                 angularAccelerationDerivEqn4, angularAccelerationDerivEqn5]

angularAccelerationDerivEqn1, angularAccelerationDerivEqn2, angularAccelerationDerivEqn3,
 angularAccelerationDerivEqn4, angularAccelerationDerivEqn5 :: Expr

angularAccelerationDerivEqn1 = sy force $= sy mass * sy acceleration

angularAccelerationDerivEqn2 = negate (sy mass * sy gravitationalAccel * sin (sy pendAngle)) $= sy mass * sy angularAccel

angularAccelerationDerivEqn3 = negate (sy gravitationalAccel * sin (sy pendAngle)) $= sy angularAccel
                              

angularAccelerationDerivEqn4 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                              $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                                 + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)

angularAccelerationDerivEqn5 = sy tension * cos (sy pendAngle) * sy unitVect - sy tension * sin (sy pendAngle) - sy mass * sy gravitationalAccel * sy unitVectj
                              $= sy mass * sy lenRod * ( sy angularAccel * cos (sy pendAngle) * sy unitVect - square (sy angularVelocity) * sin (sy pendAngle)
                               + sy angularAccel * sin (sy pendAngle) * sy unitVectj + square (sy angularVelocity) * cos (sy pendAngle) * sy unitVectj)


-- Notes

angleConstraintNote :: Sentence


angleConstraintNote = S "The" +:+ phrase constraint +:+
     E ( sy pendAngle $> 0) `sIs` S "required"

--gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
--   landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
--   timeConsNote, tolNote :: Sentence