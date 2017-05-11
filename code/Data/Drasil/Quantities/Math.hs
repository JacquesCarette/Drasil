module Data.Drasil.Quantities.Math where
import Data.Drasil.SI_Units

import Language.Drasil
import Data.Drasil.Concepts.Math as CM

diameter, gradient, normalVect, orientation, unitVect, euclidNorm, perpVect :: ConVar

diameter = cvR CM.diameter lD
gradient  = cvR CM.gradient (Greek Nabla)
normalVect = cvR CM.normalV (vec $ lN)
unitVect = cvR CM.unitV (vec $ hat lI)
perpVect = cvR perpV (vec $ lN)
orientation = cvR CM.orient (Greek Phi_L)
euclidNorm = cvR CM.euclidN (Concat [Atomic "||", (vec lR), Atomic "||"])

