module Drasil.Projectile.References (citations) where

import Language.Drasil
import Data.Drasil.Citations (accelerationWiki, velocityWiki,
 hibbeler2004, parnasClements1986)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004,
             parnasClements1986]
