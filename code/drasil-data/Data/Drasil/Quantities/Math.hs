module Data.Drasil.Quantities.Math where

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (mkQuantDef)

import qualified Data.Drasil.Concepts.Math as CM (area, diameter, euclidN, gradient, 
    normalV, orient, perpV, pi_, surArea, surface, unitV)
import Data.Drasil.SI_Units (metre, m_2, radian)

gradient, normalVect, unitVect, unitVectj, euclidNorm, perpVect, pi_, 
  uNormalVect :: DefinedQuantityDict

gradient    = dqdMayUnit CM.gradient lNabla         Real Nothing
normalVect  = dqdMayUnit CM.normalV  (vec lN)       Real Nothing
uNormalVect = dqdMayUnit CM.normalV  (vec $ hat lN) Real Nothing
unitVect    = dqdMayUnit CM.unitV    (vec $ hat lI) Real Nothing
unitVectj   = dqdMayUnit CM.unitV    (vec $ hat lJ) Real Nothing
perpVect    = dqdMayUnit CM.perpV    (vec lN)       Real Nothing
pi_         = dqdMayUnit CM.pi_      lPi            Real Nothing
euclidNorm  = dqdMayUnit CM.euclidN  (Concat [Atomic "||", vec lR, Atomic "||"])
                                                        Real Nothing  

area, diameter, surface, surArea, orientation :: UnitalChunk

area        = ucs' CM.area     cA   Real    m_2
diameter    = ucs' CM.diameter lD   Real    metre
surface     = ucs' CM.surface  cS   Real    m_2
surArea     = ucs' CM.surArea  cA   Real    m_2
orientation = ucs' CM.orient   lPhi Radians radian

-- Constants

piConst :: QDefinition
piConst = mkQuantDef pi_ (Dbl 3.14159265)
