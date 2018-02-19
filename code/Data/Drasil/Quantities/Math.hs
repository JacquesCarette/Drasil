module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: ConVar

diameter = cv CM.diameter lD Real
gradient  = cv CM.gradient (Greek Nabla) Real
normalVect = cv CM.normalV (vec $ lN) Real
uNormalVect = cv CM.normalV (vec $ hat lN) Real
unitVect = cv CM.unitV (vec $ hat lI) Real
perpVect = cv perpV (vec $ lN) Real
surface = cv CM.surface cS Real
euclidNorm = cv CM.euclidN (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea = ucs' CM.surArea cA m_2 Real
orientation = ucs' CM.orient (Greek Phi_L) radian Radians
