module Data.Drasil.Quantities.Math where

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Math as CM (area, diameter, euclidN, gradient, 
    normalV, orient, perpV, pi_, surArea, surface, unitV)
import Data.Drasil.SI_Units (metre, m_2, radian)

gradient, normalVect, unitVect, euclidNorm, perpVect, pi_, uNormalVect :: DefinedQuantityDict

gradient    = dqd' CM.gradient (const $ lNabla)        Real Nothing
normalVect  = dqd' CM.normalV  (const $ vec $ lN)      Real Nothing
uNormalVect = dqd' CM.normalV  (const $ vec $ hat lN)  Real Nothing
unitVect    = dqd' CM.unitV    (const $ vec $ hat lI)  Real Nothing
perpVect    = dqd' CM.perpV    (const $ vec $ lN)      Real Nothing
pi_         = dqd' CM.pi_      (const $ lPi)           Real Nothing
euclidNorm  = dqd' CM.euclidN  (const $ Concat [Atomic "||", (vec lR), Atomic "||"])
                                                        Real Nothing  

area, diameter, surface, surArea, orientation :: UnitalChunk

area        = ucs' CM.area     cA   Real    m_2
diameter    = ucs' CM.diameter lD   Real    metre
surface     = ucs' CM.surface  cS   Real    m_2
surArea     = ucs' CM.surArea  cA   Real    m_2
orientation = ucs' CM.orient   lPhi Radians radian
