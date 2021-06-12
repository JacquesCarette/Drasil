module Drasil.Projectile.Lesson.IntroSection where

import Data.Drasil.Concepts.Physics (force, motion)
import Drasil.Projectile.Concepts (projectile, projMotion)

import Utils.Drasil (foldlSent)
import Language.Drasil

introPara :: Sentence
introPara
  = foldlSent
      [S "One of the first topics typically taught when students are introduced",
         S "to the study of dynamics is" +:+. phrase projMotion, 
         atStart projMotion, S "is a good starting point for two reasons:",
         S "1. The model developed is relatively simple because it only involves kinematic quantities,",
         S ", not kinetics. Kinematics focuses on the geometrics aspects of", phrase motion,
         S "while kinetics introduces the concept of" +:+. plural force, 
       S "2. Projectile motion has practical applications for tracking the", phrase motion, 
         S "of launched or thrown objects"]