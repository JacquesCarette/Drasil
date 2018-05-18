module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: DefinedQuantityDict

diameter    = cqsEL CM.diameter lD                                            Real
gradient    = cqsEL CM.gradient (Greek Nabla)                                 Real
normalVect  = cqsEL CM.normalV  (vec $ lN)                                    Real
uNormalVect = cqsEL CM.normalV  (vec $ hat lN)                                Real
unitVect    = cqsEL CM.unitV    (vec $ hat lI)                                Real
perpVect    = cqsEL CM.perpV    (vec $ lN)                                    Real
surface     = cqsEL CM.surface  cS                                            Real
euclidNorm  = cqsEL CM.euclidN  (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea     = ucs' CM.surArea cA            m_2    Real []
orientation = ucs' CM.orient  (Greek Phi_L) radian Radians []
