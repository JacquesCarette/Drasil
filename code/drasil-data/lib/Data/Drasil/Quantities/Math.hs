-- | Assigns a symbol and possibly units (quantities) to mathematical quantities.
module Data.Drasil.Quantities.Math where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators (combineNINI)
import Drasil.Database (mkUid)
import Language.Drasil.Display (Symbol(Atop), Decoration(Magnitude))
import Language.Drasil.ShortHands

import qualified Data.Drasil.Concepts.Math as CM (area, diameter, norm, orient,
    surArea, surface)
import Data.Drasil.Concepts.Math (euclidSpace, normal, perp, unit_, vector)
import Data.Drasil.SI_Units (metre, m_2, radian)

-- * May Not Have Units

mathquants :: [DefinedQuantityDict]
mathquants = [gradient, normalVect, unitVect, perpVect,
  pi_, posInf, negInf, euclidNorm]

mathunitals :: [DefinedQuantityDict]
mathunitals = [area, diameter, surface, surArea, orientation]

gradient, normalVect, unitVect, unitVectj, euclidNorm, perpVect,
  pi_, posInf, negInf, uNormalVect :: DefinedQuantityDict

gradient    = quantNoUnit' (mkUid "gradient")      (cn' "gradient")
  (S "degree of steepness of a graph at any point")
  (const lNabla) Real
normalVect  = quantNoUnit' (mkUid "normal vector") (combineNINI normal vector)
  (S "unit outward normal vector for a surface")
  (const $ vec lN) Real
uNormalVect = quantNoUnit' (mkUid "normal vector") (combineNINI normal vector)
  (S "unit outward normal vector for a surface")
  (const $ vec $ hat lN) Real
unitVect    = quantNoUnit' (mkUid "unit_vect")     (combineNINI unit_ vector)
  (S "a vector that has a magnitude of one")
  (const $ vec $ hat lI) Real
unitVectj   = quantNoUnit' (mkUid "unit_vect")     (combineNINI unit_ vector)
  (S "a vector that has a magnitude of one")
  (const $ vec $ hat lJ) Real
perpVect    = quantNoUnit' (mkUid "perp_vect")     (combineNINI perp vector)
  (S "vector perpendicular or 90 degrees to another vector")
  (const $ vec lN) Real
pi_    = quantNoUnit' (mkUid "pi")     (cn' "ratio of circumference to diameter for any circle")
           (S "The ratio of a circle's circumference to its diameter")
           (staged lPi (variable "pi")) Real
posInf = quantNoUnit' (mkUid "PosInf") (cn' "Positive Infinity")
           (S "the limit of a sequence or function that eventually exceeds any prescribed bound")
           (staged lPosInf (variable "posInf")) Real
negInf = quantNoUnit' (mkUid "NegInf") (cn' "Negative Infinity")
           (S "Opposite of positive infinity")
           (staged lNegInf (variable "posInf")) Real
euclidNorm  = quantNoUnit' (mkUid "euclidNorm")    (combineNINI euclidSpace CM.norm)
  (S "euclidean norm")
  (const $ Atop Magnitude $ vec lD) Real

-- * With Units

area, diameter, surface, surArea, orientation :: DefinedQuantityDict

area        = dqd CM.area     cA   Real m_2
diameter    = dqd CM.diameter lD   Real metre
surface     = dqd CM.surface  cS   Real m_2
surArea     = dqd CM.surArea  cA   Real m_2
orientation = dqd CM.orient   lPhi Real radian

-- * Constants

piConst :: ConstQDef
piConst = mkQuantDef pi_ (dbl 3.14159265)
