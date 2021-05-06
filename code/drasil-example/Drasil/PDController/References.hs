module Drasil.PDController.References where
import Data.Drasil.Citations (smithLai2005)
import Data.Drasil.People (wikiAuthors)

import Language.Drasil

citations :: BibRef
citations = [pidWiki, laplaceWiki, johnson2008, abbasi2015, smithLai2005]

pidWiki, johnson2008, abbasi2015, laplaceWiki :: Citation

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
