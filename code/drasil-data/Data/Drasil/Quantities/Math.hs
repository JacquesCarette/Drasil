module Data.Drasil.Quantities.Math where

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (mkQuantDef)

import qualified Data.Drasil.Concepts.Math as CM (area, diameter, euclidN, gradient, 
    normalV, orient, perpV, pi_, surArea, surface, unitV)
import Data.Drasil.SI_Units (metre, m_2, radian)

gradient, normalVect, unitVect, unitVectj, euclidNorm, perpVect,
  pi_, uNormalVect :: DefinedQuantityDict

gradient    = dqdNoUnit CM.gradient lNabla         Real
normalVect  = dqdNoUnit CM.normalV  (vec lN)       Real
uNormalVect = dqdNoUnit CM.normalV  (vec $ hat lN) Real
unitVect    = dqdNoUnit CM.unitV    (vec $ hat lI) Real
unitVectj   = dqdNoUnit CM.unitV    (vec $ hat lJ) Real
perpVect    = dqdNoUnit CM.perpV    (vec lN)       Real
pi_         = dqd'      CM.pi_      (staged lPi (Variable "pi")) Real Nothing
euclidNorm  = dqdNoUnit CM.euclidN  (Concat [Label "||", vec lR, Label "||"]) Real  

area, diameter, surface, surArea, orientation :: UnitalChunk

area        = ucs' CM.area     cA   Real    m_2
diameter    = ucs' CM.diameter lD   Real    metre
surface     = ucs' CM.surface  cS   Real    m_2
surArea     = ucs' CM.surArea  cA   Real    m_2
orientation = ucs' CM.orient   lPhi Radians radian

-- Constants

piConst :: QDefinition
piConst = mkQuantDef pi_ (Dbl 3.14159265)
