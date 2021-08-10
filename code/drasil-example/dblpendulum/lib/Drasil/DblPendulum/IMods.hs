{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.IMods (iMods, angularDisIM_1, angularDisIM_2) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia,
         time, angularDisplacement, angularFrequency, torque, angularDisplacement, time)
import Data.Drasil.Concepts.Math (constraint, equation, amplitude, iAngle, angle)
import Data.Drasil.Concepts.Physics (pendulum, motion, shm)
import Data.Drasil.Theories.Physics (newtonSLR)

import Drasil.DblPendulum.Expressions (angularDisplacementExpr, angularDisplacementDerivEqns, angularDisExpr_1, angularDisExpr_2)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle, initialPendAngle, 
  pendDisAngle_1, lenRod_1, lenRod_2, pendDisAngle_2, massObj_1, massObj_2, 
  angularAccel_1, angularAccel_2, angularVel_1, angularVel_2)

iMods :: [InstanceModel]
iMods = [angularDisIM_1, angularDisIM_2]

-- -- Angular Displacement
-- angularDisplacementIM :: InstanceModel
-- angularDisplacementIM = imNoRefs angularDisplacementMK
--   [qwC lenRod $ UpFrom (Exc, exactDbl 0)
--   ,qwC initialPendAngle $ UpFrom (Exc, exactDbl 0)
--   , qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)]
--   (qw pendDisplacementAngle) [UpFrom (Exc, exactDbl 0)]
--   (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angularDispConstraintNote]

-- angularDisplacementMK :: ModelKind 
-- angularDisplacementMK = equationalModel "angularDisplacementIM"
--   (nounPhraseSP "calculation of angular displacement") angularDisplacementFD

-- angularDisplacementFD :: QDefinition
-- angularDisplacementFD = mkFuncDefByQ pendDisplacementAngle [time] angularDisplacementExpr

-- angularDisplacementDeriv :: Derivation
-- angularDisplacementDeriv = mkDerivName (phrase angularDisplacement) (weave [angularDisplacementDerivSents, map eS angularDisplacementDerivEqns])

-- angularDisplacementDerivSents :: [Sentence]
-- angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
--                              angularDisplacementDerivSent4, angularDisplacementDerivSent5]

-- angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
--   angularDisplacementDerivSent4, angularDisplacementDerivSent5 :: Sentence
-- angularDisplacementDerivSent1 = foldlSentCol [S "When", phraseNP (the pendulum) `S.is` S "displaced to an", phrase iAngle `S.and_` S "released" `sC`
--                                        phraseNP (the pendulum), S "swings back and forth with periodic" +:+. phrase motion,
--                                        S "By applying", namedRef newtonSLR (phrase newtonSLR) `sC`
--                                        phraseNP (NP.the (equation `of_` motion) `NP.for` the pendulum), S "may be obtained"]
-- angularDisplacementDerivSent2 = foldlSentCol [S "Where", ch torque `S.denotes` phrase torque `sC`
--                                     ch momentOfInertia `S.denotes` phrase momentOfInertia `S.and_` ch angularAccel `S.denotes`
--                                     (phrase angularAccel !.), S "This implies"]
-- angularDisplacementDerivSent3 = foldlSentCol [S "And rearranged as" ]
-- angularDisplacementDerivSent4 = foldlSentCol [S "If", phraseNP (NP.the (amplitude `of_` angularDisplacement)), S "is small enough" `sC`
--   S "we can approximate", eS (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
--   S "at very small" +:+. plural angle,
--   S "Then", phraseNP (NP.the (equation `of_` motion)), S "reduces to", phraseNP (NP.the (equation `of_` shm))]                                       
-- angularDisplacementDerivSent5 = foldlSentCol [S "Thus the", phrase shm, S "is" ]

-- angularDispConstraintNote :: Sentence
-- angularDispConstraintNote = foldlSent [atStartNP (the constraint),
--      eS (sy initialPendAngle $> exactDbl 0) `S.is` (S "required" !.),
--      atStartNP (the angularFrequency) `S.is` definedIn'' angFrequencyGD]


angularDisIM_1 :: InstanceModel
angularDisIM_1 = imNoRefs angularDisMK_1
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw angularAccel_1) [UpFrom (Exc, exactDbl 0)]
  (Just angularDisDeriv_1) "calOfAngularDisplacement1" []

angularDisMK_1 :: ModelKind 
angularDisMK_1 = equationalModel "angularDisplacementIM1"
  (nounPhraseSP "calculation of angular displacement") angularDisFD_1

angularDisFD_1 :: QDefinition
angularDisFD_1 = mkFuncDefByQ angularAccel_1
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularDisExpr_1

angularDisDeriv_1 :: Derivation
angularDisDeriv_1 = mkDerivName (phrase pendDisAngle_1) (weave [])

angularDisIM_2 :: InstanceModel
angularDisIM_2 = imNoRefs angularDisMK_2
  [qwC lenRod_1 $ UpFrom (Exc, exactDbl 0),
   qwC lenRod_2 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_1 $ UpFrom (Exc, exactDbl 0),
   qwC massObj_2 $ UpFrom (Exc, exactDbl 0),
   qwUC pendDisAngle_1,
   qwUC pendDisAngle_2]
  (qw angularAccel_2) [UpFrom (Exc, exactDbl 0)]
  (Just angularDisDeriv_2) "calOfAngularDisplacement2" []

angularDisMK_2 :: ModelKind 
angularDisMK_2 = equationalModel "angularDisplacementIM2"
  (nounPhraseSP "calculation of angular displacement") angularDisFD_2

angularDisFD_2 :: QDefinition
angularDisFD_2 = mkFuncDefByQ angularAccel_2
  [pendDisAngle_1, pendDisAngle_2, angularVel_1, angularVel_2] angularDisExpr_2

angularDisDeriv_2 :: Derivation
angularDisDeriv_2 = mkDerivName (phrase pendDisAngle_2) (weave [])
