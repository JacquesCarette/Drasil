module Drasil.Projectile.Lesson.IntroSection where

import Data.Drasil.Concepts.Physics (force, motion)
import Drasil.Projectile.Concepts (projectile, projMotion)

import Utils.Drasil (foldlSP, foldlSP_, foldlSent, enumBulletU)
import qualified Utils.Drasil.Sentence as S
import Language.Drasil

introContext :: Contents
introContext = foldlSP_ 
  [S "One of the first topics typically taught when students are introduced to the study of dynamics is"
   +:+. phrase projMotion, atStart projMotion +:+ S "is a good starting point for two reasons:"]

reasonList :: Contents
reasonList = enumBulletU $ map foldlSent
  [[S "The model developed is relatively simple because it only involves kinematic quantities,",
    S "not kinetics. Kinematics focuses on the geometrics aspects of", phrase motion,
    S "while kinetics introduces the concept of" +:+ plural force], 
  [atStart projMotion +:+ S "has practical applications for tracking the", phrase motion, 
    S "of launched or thrown objects"]]

overviewParagraph :: Contents
overviewParagraph = foldlSP [S "The presentation below is based on Section 12.6 (", atStart motion `S.of_`
  S "a", atStart projectile, S") from the classic Hibbler text" +:+
  Quote (S "Engineering Mechanics Dynamnics, 10th edition")]