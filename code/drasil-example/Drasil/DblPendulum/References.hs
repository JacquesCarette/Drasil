module Drasil.DblPendulum.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki)
import Drasil.Projectile.References (hibbeler2004)


citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki]

