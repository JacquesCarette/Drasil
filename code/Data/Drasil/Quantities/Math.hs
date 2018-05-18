module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: DefinedQuantityDict

diameter    = dqdEL CM.diameter lD                                            Real
gradient    = dqdEL CM.gradient (Greek Nabla)                                 Real
normalVect  = dqdEL CM.normalV  (vec $ lN)                                    Real
uNormalVect = dqdEL CM.normalV  (vec $ hat lN)                                Real
unitVect    = dqdEL CM.unitV    (vec $ hat lI)                                Real
perpVect    = dqdEL CM.perpV    (vec $ lN)                                    Real
surface     = dqdEL CM.surface  cS                                            Real
euclidNorm  = dqdEL CM.euclidN  (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea     = ucs' CM.surArea cA            m_2    Real
orientation = ucs' CM.orient  (Greek Phi_L) radian Radians
