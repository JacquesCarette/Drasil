module Drasil.Projectile.References where

import Language.Drasil

citations :: BibRef
citations = [accelerationWiki, velocityWiki]

accelerationWiki, velocityWiki :: Citation

accelerationWiki = cMisc [author [mononym "Wikipedia Contributors"],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [mononym "Wikipedia Contributors"],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"
