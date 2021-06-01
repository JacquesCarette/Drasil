module Drasil.ProjectileLesson.Body where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)

-- **** Add export parameters in a module
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkDoc)
import Drasil.DocDecl (NBDecl, DocSection'(Bibliography, IntroSec, BodySec, SmmrySec))
import Drasil.DocumentLanguage.Notebook.Core(IntroSec(..), BodySec(..), SmmrySec(..))

import Drasil.ProjectileLesson.IntroSection (introPara)
import Drasil.ProjectileLesson.Review (reviewContextP1, reviewEq, reviewContextP2)

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
          Motion [gsduserCharacteristics], MethsAndAnls [] []],
  Bibliography
  ]

projMotionLesson :: CI
projMotionLesson = commonIdea "projMotionLesson" (pn "Projectile Motion Lesson") "Projectile Motion lesson" []