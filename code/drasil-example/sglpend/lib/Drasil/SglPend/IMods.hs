{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.IMods (iMods, angularDisplacementIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil (weave)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.NounPhrase.Combinators as NP
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia,
         time, angularDisplacement, angularFrequency, torque, angularDisplacement, time)
import Data.Drasil.Concepts.Math (constraint, equation, amplitude, iAngle, angle)
import Data.Drasil.Concepts.Physics (pendulum, motion, shm)
import Data.Drasil.Theories.Physics (newtonSLR)
import Drasil.SglPend.GenDefs (angFrequencyGD)

import Drasil.SglPend.Derivations (angularDisplacementDerivEqns)
import Drasil.SglPend.Expressions (angularDisplacementExpr)
import Drasil.SglPend.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)

iMods :: [InstanceModel]
iMods = [angularDisplacementIM]

-- Angular Displacement
angularDisplacementIM :: InstanceModel
angularDisplacementIM = imNoRefs angularDisplacementMK
  [qwC lenRod $ UpFrom (Exc, exactDbl 0)
  ,qwC initialPendAngle $ UpFrom (Exc, exactDbl 0)
  , qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)]
  (qw pendDisplacementAngle) [UpFrom (Exc, exactDbl 0)]
  (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angularDispConstraintNote]

angularDisplacementMK :: ModelKind Expr
angularDisplacementMK = equationalModel "angularDisplacementIM"
  (nounPhraseSP "calculation of angular displacement") angularDisplacementFD

angularDisplacementFD :: SimpleQDef
angularDisplacementFD = mkFuncDefByQ pendDisplacementAngle
  [time] angularDisplacementExpr

angularDisplacementDeriv :: Derivation
angularDisplacementDeriv = mkDerivName (phrase angularDisplacement) (weave [angularDisplacementDerivSents, map eS angularDisplacementDerivEqns])

angularDisplacementDerivSents :: [Sentence]
angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
                             angularDisplacementDerivSent4, angularDisplacementDerivSent5]

angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
  angularDisplacementDerivSent4, angularDisplacementDerivSent5 :: Sentence
angularDisplacementDerivSent1 = foldlSentCol [S "When", phraseNP (the pendulum) `S.is` S "displaced to an", phrase iAngle `S.and_` S "released" `sC`
                                       phraseNP (the pendulum), S "swings back and forth with periodic" +:+. phrase motion,
                                       S "By applying", namedRef newtonSLR (phrase newtonSLR) `sC`
                                       phraseNP (NP.the (equation `of_` motion) `NP.for` the pendulum), S "may be obtained"]
angularDisplacementDerivSent2 = foldlSentCol [S "Where", ch torque `S.denotes` phrase torque `sC`
                                    ch momentOfInertia `S.denotes` phrase momentOfInertia `S.and_` ch angularAccel `S.denotes`
                                    (phrase angularAccel !.), S "This implies"]
angularDisplacementDerivSent3 = foldlSentCol [S "And rearranged as" ]
angularDisplacementDerivSent4 = foldlSentCol [S "If", phraseNP (NP.the (amplitude `of_` angularDisplacement)), S "is small enough" `sC`
  S "we can approximate", eS (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
  S "at very small" +:+. plural angle,
  S "Then", phraseNP (NP.the (equation `of_` motion)), S "reduces to", phraseNP (NP.the (equation `of_` shm))]                                       
angularDisplacementDerivSent5 = foldlSentCol [S "Thus the", phrase shm, S "is" ]

angularDispConstraintNote :: Sentence
angularDispConstraintNote = foldlSent [atStartNP (the constraint),
     eS (sy initialPendAngle $> exactDbl 0) `S.is` (S "required" !.),
     atStartNP (the angularFrequency) `S.is` definedIn'' angFrequencyGD]
