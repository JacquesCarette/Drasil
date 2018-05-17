module Data.Drasil.Quantities.Math where


import Language.Drasil
import Data.Drasil.Concepts.Math as CM
import Data.Drasil.SI_Units

diameter, gradient, normalVect, unitVect, euclidNorm, perpVect, surface, uNormalVect :: DefinedQuantityDict

diameter    = cqs CM.diameter lD                                            Real
gradient    = cqs CM.gradient (Greek Nabla)                                 Real
normalVect  = cqs CM.normalV  (vec $ lN)                                    Real
uNormalVect = cqs CM.normalV  (vec $ hat lN)                                Real
unitVect    = cqs CM.unitV    (vec $ hat lI)                                Real
perpVect    = cqs CM.perpV    (vec $ lN)                                    Real
surface     = cqs CM.surface  cS                                            Real
euclidNorm  = cqs CM.euclidN  (Concat [Atomic "||", (vec lR), Atomic "||"]) Real


surArea, orientation :: UnitalChunk

surArea     = ucs' CM.surArea cA            m_2    Real []
orientation = ucs' CM.orient  (Greek Phi_L) radian Radians []
