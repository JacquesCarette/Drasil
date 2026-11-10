module Drasil.PDController.References (
  citations,
  johnson2008, abbasi2015, pdControllerOriginal
) where
import Data.Drasil.Citations (laplaceWiki, pidWiki)
import Data.Drasil.People (naveen)

import Language.Drasil
import Language.Drasil.Document

citations :: BibRef
citations = [johnson2008, abbasi2015, laplaceWiki, pidWiki, pdControllerOriginal]

johnson2008, abbasi2015, pdControllerOriginal :: Citation

pidCtrlEditor1, pidCtrlEditor2 :: Person
pidCtrlEditor1 = personWM "Michael" ["A"] "Johnson"
pidCtrlEditor2 = personWM "Mohammad" ["H"] "Moradi"

johnson2008
  = cBookA [pidCtrlEditor1, pidCtrlEditor2]
      "PID Control: New Identification and Design Methods, Chapter 1"
      "Springer Science and Business Media"
      2006
      []
      "johnson2008"

abbasi2015
  = cMisc
      [author [personWM "Nasser" ["M"] "Abbasi"],
       title "A differential equation view of closed loop control systems",
       howPublishedU
         "https://www.12000.org/my_notes/connecting_systems/report.htm",
       month Nov, year 2020]
      "abbasi2015"

-- | The original, manually created version of PD Controller.
pdControllerOriginal = cMisc
  [ author [naveen]
  , title "Software Requirements Specification for PD Controller: Simulation of a PID control loop"
  , howPublishedU "https://github.com/muralidn/CAS741-Fall20/tree/master"
  ]
  "pdControllerOriginal"
