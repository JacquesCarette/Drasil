module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM (diameter, euclidN, gradient, normalV,
    orient, perpV, surArea, surface, unitV)
import Data.Drasil.SI_Units (metre, m_2, radian)

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: DefinedQuantityDict

diameter    = dqdEL CM.diameter lD                      Real metre
gradient    = dqd'  CM.gradient (const $ lNabla)        Real Nothing
normalVect  = dqd'  CM.normalV  (const $ vec $ lN)      Real Nothing
uNormalVect = dqd'  CM.normalV  (const $ vec $ hat lN)  Real Nothing
unitVect    = dqd'  CM.unitV    (const $ vec $ hat lI)  Real Nothing
perpVect    = dqd'  CM.perpV    (const $ vec $ lN)      Real Nothing
surface     = dqdEL CM.surface  cS                      Real m_2
euclidNorm  = dqd'  CM.euclidN  (const $ Concat [Atomic "||", (vec lR), Atomic "||"])
                                                        Real Nothing  

pi_ :: QuantityDict
pi_         = mkQuant "pi"   (pn "pi") lPi              Real Nothing Nothing

surArea, orientation :: UnitalChunk

surArea     = ucs' CM.surArea cA            m_2    Real
orientation = ucs' CM.orient  lPhi radian Radians
