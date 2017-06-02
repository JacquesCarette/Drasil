module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: ConVar

diameter = cvR CM.diameter lD
gradient  = cvR CM.gradient (Greek Nabla)
normalVect = cvR CM.normalV (vec $ lN)
uNormalVect = cvR CM.normalV (vec $ hat lN)
unitVect = cvR CM.unitV (vec $ hat lI)
perpVect = cvR perpV (vec $ lN)
surface = cvR CM.surface cS
euclidNorm = cvR CM.euclidN (Concat [Atomic "||", (vec lR), Atomic "||"])


surArea, orientation :: UnitalChunk

surArea = uc CM.surArea cA m_2
orientation = uc CM.orient (Greek Phi_L) radian