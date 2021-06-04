module Drasil.Projectile.Lesson.Body where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)

-- **** Add export parameters in a module
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkDoc)
import Drasil.DocDecl (NBDecl, DocSection'(Bibliography, IntroSec, BodySec, SmmrySec))
import Drasil.DocumentLanguage.Notebook.Core(IntroSec(..), BodySec(..), SmmrySec(..))

import Drasil.Projectile.Lesson.IntroSection (introPara)
import Drasil.Projectile.Lesson.Review (reviewContextP1, reviewEq, reviewContextP2)
import Drasil.Projectile.Lesson.Motion (motionContextP1, motionContextP2, horMotion, verMotion, summary)

nb :: Document
nb = mkDoc mkNB (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkNB :: NBDecl
mkNB = [
  IntroSec $
    IntroProg introPara (phrase projMotionLesson)
  BodySec $
       BodyProg
         [Review [reviewContextP1, reviewEq, reviewContextP2],
          Motion [motionContextP1, motionContextP2] [horMotion, verMotion, summary],
          MethsAndAnls [mAndaintro] []],
  Bibliography
  ]

projMotionLesson :: CI
projMotionLesson = commonIdea "projMotionLesson" (pn "Projectile Motion Lesson") "Projectile Motion lesson" []

mAndaintro :: Contents
mAndaintro = foldlSP 
  [S "Free-flight projectile motion problems can be solved using the following procedure."]