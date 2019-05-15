module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Language.Drasil.ShortHands (lM, cL, cV, lGamma, lRho)

import Data.Drasil.Concepts.PhysicalProperties as CPP (density, specWeight, len,
  mass, vol)
import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight)
import Data.Drasil.Units.PhysicalProperties (densityU)

density, specWeight, mass, len, vol :: UnitalChunk
density    = uc CPP.density    lRho   densityU
specWeight = uc CPP.specWeight lGamma specificWeight
mass       = uc CPP.mass       lM     kilogram
len        = uc CPP.len        cL     metre
vol        = uc CPP.vol        cV     m_3
