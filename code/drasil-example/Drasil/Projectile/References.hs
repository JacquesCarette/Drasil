module Drasil.Projectile.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki)
import Data.Drasil.People (rcHibbeler, wikiAuthors)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki]

accelerationWiki, velocityWiki, hibbeler2004 :: Citation

accelerationWiki = cMisc [author [wikiAuthors],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [wikiAuthors],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"

hibbeler2004 = cBookA [rcHibbeler] "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall" 2004 []
  "hibbeler2004"
