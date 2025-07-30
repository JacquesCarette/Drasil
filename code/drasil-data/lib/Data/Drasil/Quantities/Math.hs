-- | Assigns a symbol and possibly units (quantities) to mathematical quantities.
module Data.Drasil.Quantities.Math where

import Language.Drasil
import Language.Drasil.Display
import Language.Drasil.ShortHands

import qualified Data.Drasil.Concepts.Math as CM (area, diameter, euclidN, gradient, 
    normalV, orient, perpV, pi_, posInf, negInf, surArea, surface, unitV)
import Data.Drasil.SI_Units (metre, m_2, radian)

-- * May Not Have Units

mathquants :: [DefinedQuantityDict]
mathquants = [gradient, normalVect, unitVect, perpVect,
  pi_, posInf, negInf, euclidNorm]

mathunitals :: [UnitalChunk]
mathunitals = [area, diameter, surface, surArea, orientation]

gradient, normalVect, unitVect, unitVectj, euclidNorm, perpVect,
  pi_, posInf, negInf, uNormalVect :: DefinedQuantityDict
 

gradient    = dqdNoUnit CM.gradient lNabla         Real
normalVect  = dqdNoUnit CM.normalV  (vec lN)       Real
uNormalVect = dqdNoUnit CM.normalV  (vec $ hat lN) Real
unitVect    = dqdNoUnit CM.unitV    (vec $ hat lI) Real
unitVectj   = dqdNoUnit CM.unitV    (vec $ hat lJ) Real
perpVect    = dqdNoUnit CM.perpV    (vec lN)       Real
pi_         = dqd'      CM.pi_      (staged lPi (variable "pi")) Real Nothing
posInf      = dqd'      CM.posInf   (staged lPosInf (variable "posInf")) Real Nothing
negInf      = dqd'      CM.negInf   (staged lNegInf (variable "posInf")) Real Nothing
euclidNorm  = dqdNoUnit CM.euclidN  (Atop Magnitude $ vec lD) Real

-- * With Units

area, diameter, surface, surArea, orientation :: UnitalChunk

area        = uc CM.area     cA   Real m_2
diameter    = uc CM.diameter lD   Real metre
surface     = uc CM.surface  cS   Real m_2
surArea     = uc CM.surArea  cA   Real m_2
orientation = uc CM.orient   lPhi Real radian

-- * Constants

piConst :: ConstQDef
piConst = mkQuantDef pi_ (dbl 3.14159265)
