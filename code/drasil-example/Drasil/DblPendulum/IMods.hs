module Drasil.DblPendulum.IMods (iMods, angularDisplacementIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
  --imNoDerivNoRefs, )
import Utils.Drasil
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia,
         time, angularDisplacement, angularFrequency, torque, angularDisplacement, time)
--import Data.Drasil.Theories.Physics (newtonSL)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle , initialPendAngle)
import Data.Drasil.Concepts.Math (constraint, equation)
import Data.Drasil.Concepts.Physics (pendulum)
import Drasil.DblPendulum.TMods (newtonSLR)


iMods :: [InstanceModel]
iMods = [angularDisplacementIM]

---
angularDisplacementIM :: InstanceModel
angularDisplacementIM = imNoRefs angularDisplacementRC 
  [qwC lenRod $ UpFrom (Exc, 0)
  ,qwC initialPendAngle $ UpFrom (Exc, 0)
  , qwC gravitationalAccel $ UpFrom (Exc, 0)]
  (qw angularDisplacement) [UpFrom (Exc, 0)]
  (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angleConstraintNote]
  

angularDisplacementRC :: RelationConcept
angularDisplacementRC = makeRC "angularDisplacementRC" (nounPhraseSP "calculation of angular acceleration")
  EmptyS $ apply1 angularDisplacement time $= sy initialPendAngle * cos ( sy angularFrequency * sy time)
  

angularDisplacementDeriv :: Derivation 
angularDisplacementDeriv = mkDerivName (phrase angularAccel) (weave [angularDisplacementDerivSents, map E angularAccelerationDerivEqns])

angularDisplacementDerivSents :: [Sentence]
angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
                             angularDisplacementDerivSent4, angularDisplacementDerivSent5]

angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
  angularDisplacementDerivSent4, angularDisplacementDerivSent5 :: Sentence

angularDisplacementDerivSent1 = foldlSentCol [S "When the", phrase pendulum `sIs` S "displaced to an initial angle and released" `sC`
                                       S "the", phrase pendulum, S "swings back and forth with periodic motion" +:+
                                       S "By applying Newton's Second Law for Rotation" `sIn` makeRef2S newtonSLR `sC`
                                       S "the equation of motion for the", phrase pendulum, S "may be obtained"]
       
 
angularDisplacementDerivSent2 = foldlSentCol [S "Where", ch torque +:+ S "denotes the", phrase torque `sC`
                                    ch momentOfInertia +:+ S "denotes the", phrase momentOfInertia `sAnd` ch angularAccel +:+ 
                                    S "denotes the", phrase angularAccel +:+  S "This implies"]
                 

angularDisplacementDerivSent3 = foldlSentCol [S "And rearranged as" ] 

angularDisplacementDerivSent4 = foldlSentCol [S "If the amplitude of", phrase angularDisplacement, S "is small enough" `sC`
  S "we can approximate", E (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
  S "at very small angles." :+:
  S " Then the", phrase equation, S "of motion reduces to the", phrase equation, S "of simple harmonic motion"]                                       

angularDisplacementDerivSent5 = foldlSentCol [S "Thus the simple harmonic motion is" ] 

angularAccelerationDerivEqns :: [Expr]
angularAccelerationDerivEqns = [angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
                                 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5]

angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5 :: Expr

angularDisplacementDerivEqn1 = sy torque $= sy momentOfInertia * sy angularAccel

angularDisplacementDerivEqn2 = negate (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle) * sy lenRod) $= sy mass * sy lenRod $^ 2 
                                * deriv (deriv (sy pendDisplacementAngle) time) time 
                                --deriv (deriv (sy XXX) time) time
          -- * deriv (apply1 angularDisplacement time) time                     
angularDisplacementDerivEqn3 = deriv (apply1 pendDisplacementAngle time) time + sy gravitationalAccel/ sy lenRod * sin (sy pendDisplacementAngle) $= 0

angularDisplacementDerivEqn4 = deriv (apply1 pendDisplacementAngle time) time + sy gravitationalAccel/ sy lenRod * sy pendDisplacementAngle $= 0

angularDisplacementDerivEqn5 = apply1 angularDisplacement time $= sy initialPendAngle * cos ( sy angularFrequency * sy time)

----------------------
    
 


-- Notes

angleConstraintNote :: Sentence


angleConstraintNote = S "The" +:+ phrase constraint +:+
     E ( sy pendDisplacementAngle $> 0) `sIs` S "required"

--gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
--   landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
--   timeConsNote, tolNote :: Sentence