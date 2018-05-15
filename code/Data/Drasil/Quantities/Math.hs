module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: DefinedQuantityDictCV

diameter    = dqd CM.diameter lD                                            Real
gradient    = dqd CM.gradient (Greek Nabla)                                 Real
normalVect  = dqd CM.normalV  (vec $ lN)                                    Real
uNormalVect = dqd CM.normalV  (vec $ hat lN)                                Real
unitVect    = dqd CM.unitV    (vec $ hat lI)                                Real
perpVect    = dqd CM.perpV    (vec $ lN)                                    Real
surface     = dqd CM.surface  cS                                            Real
euclidNorm  = dqd CM.euclidN  (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea     = ucs' CM.surArea cA            m_2    Real []
orientation = ucs' CM.orient  (Greek Phi_L) radian Radians []
