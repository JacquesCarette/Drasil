module Drasil.DblPendulum.IMods (iMods, angularDisplacementIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC, ModelKinds (OthModel)) 
  --imNoDerivNoRefs, )
import Utils.Drasil
import Utils.Drasil.Sentence
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia,
         time, angularDisplacement, angularFrequency, torque, angularDisplacement, time)
--import Data.Drasil.Theories.Physics (newtonSL)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)
import Data.Drasil.Concepts.Math (constraint, equation, amplitude, iAngle, angle)
import Data.Drasil.Concepts.Physics (pendulum, motion, shm)
import Data.Drasil.Theories.Physics (newtonSLR, newtonSLRRC)
import Drasil.DblPendulum.GenDefs (angFrequencyGD)


iMods :: [InstanceModel]
iMods = [angularDisplacementIM]

---
angularDisplacementIM :: InstanceModel
angularDisplacementIM = imNoRefs (OthModel angularDisplacementRC) 
  [qwC lenRod $ UpFrom (Exc, dbl 0)
  ,qwC initialPendAngle $ UpFrom (Exc, dbl 0)
  , qwC gravitationalAccel $ UpFrom (Exc, dbl 0)]
  (qw pendDisplacementAngle) [UpFrom (Exc, dbl 0)]
  (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angularDispConstraintNote]
  

angularDisplacementRC :: RelationConcept
angularDisplacementRC = makeRC "angularDisplacementRC" (nounPhraseSP "calculation of angular displacement")
  EmptyS $ apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)
  

angularDisplacementDeriv :: Derivation 
angularDisplacementDeriv = mkDerivName (phrase angularDisplacement) (weave [angularDisplacementDerivSents, map E angularDisplacementDerivEqns])

angularDisplacementDerivSents :: [Sentence]
angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
                             angularDisplacementDerivSent4, angularDisplacementDerivSent5]

angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
  angularDisplacementDerivSent4, angularDisplacementDerivSent5 :: Sentence

angularDisplacementDerivSent1 = foldlSentCol [S "When the", phrase pendulum `sIs` S "displaced to an", phrase iAngle `sAnd` S "released" `sC`
                                       S "the", phrase pendulum, S "swings back and forth with periodic" +:+. phrase motion,
                                       S "By applying", phrase newtonSLRRC `sIn` makeRef2S newtonSLR `sC`
                                       S "the", phrase equation `sOf` phrase motion, S "for the", phrase pendulum, S "may be obtained"]
       
 
angularDisplacementDerivSent2 = foldlSentCol [S "Where", ch torque +:+ S "denotes the", phrase torque `sC`
                                    ch momentOfInertia +:+ S "denotes the", phrase momentOfInertia `sAnd` ch angularAccel +:+ 
                                    S "denotes the", phrase angularAccel +:+  S "This implies"]
                 

angularDisplacementDerivSent3 = foldlSentCol [S "And rearranged as" ] 

angularDisplacementDerivSent4 = foldlSentCol [S "If the", phrase amplitude `sOf` phrase angularDisplacement, S "is small enough" `sC`
  S "we can approximate", E (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
  S "at very small" +:+. plural angle,
  S "Then the", phrase equation `sOf` phrase motion, S "reduces to the", phrase equation `sOf` phrase shm]                                       

angularDisplacementDerivSent5 = foldlSentCol [S "Thus the", phrase shm, S "is" ] 

angularDisplacementDerivEqns :: [Expr]
angularDisplacementDerivEqns = [angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
                                 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5]

angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5 :: Expr

angularDisplacementDerivEqn1 = sy torque $= sy momentOfInertia `mulRe` sy angularAccel

angularDisplacementDerivEqn2 = neg (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle) `mulRe` sy lenRod) $= sy mass `mulRe` sy lenRod $^ dbl 2 
                                `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time 
                                                   
angularDisplacementDerivEqn3 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` sy gravitationalAccel $/ sy lenRod `mulRe` sin (sy pendDisplacementAngle) $= dbl 0

angularDisplacementDerivEqn4 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` sy gravitationalAccel $/ sy lenRod `mulRe` sy pendDisplacementAngle $= dbl 0

angularDisplacementDerivEqn5 = apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)

----------------------
    
 


-- Notes

angularDispConstraintNote :: Sentence


angularDispConstraintNote = S "The" +:+ phrase constraint +:+
     E ( sy initialPendAngle $> dbl 0) `sIs` S "required" +:+.
     S "The" +:+ phrase angularFrequency `sIs` definedIn'' angFrequencyGD

--gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
--   landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
--   timeConsNote, tolNote :: Sentence