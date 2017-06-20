module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: ConVar

diameter = cvR CM.diameter lD
gradient  = cvRs CM.gradient (Greek Nabla) Real
normalVect = cvRs CM.normalV (vec $ lN) Real
uNormalVect = cvRs CM.normalV (vec $ hat lN) Real
unitVect = cvRs CM.unitV (vec $ hat lI) Real
perpVect = cvRs perpV (vec $ lN) Real
surface = cvRs CM.surface cS Real
euclidNorm = cvRs CM.euclidN (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea = ucs' CM.surArea cA m_2 Real
orientation = ucs' CM.orient (Greek Phi_L) radian Radians