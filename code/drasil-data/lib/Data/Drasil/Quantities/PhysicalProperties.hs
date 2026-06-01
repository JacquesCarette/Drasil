-- | Assigns symbols and units (quantities) to common physical properties.
module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Language.Drasil.ShortHands (lM, cL, cV, lGamma, lRho)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (density, specWeight, len,
  mass, vol)
import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight)
import Data.Drasil.Units.PhysicalProperties (densityU)

physicalquants :: [UnitalChunk]
physicalquants = [density, specWeight, mass, len, vol]

density, specWeight, mass, len, vol :: UnitalChunk
density    = uc CPP.density    lRho   Real densityU
specWeight = uc CPP.specWeight lGamma Real specificWeight
mass       = uc CPP.mass       lM     Real kilogram
len        = uc CPP.len        cL     Real metre
vol        = uc CPP.vol        cV     Real m_3
