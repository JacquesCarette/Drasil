-- | Assigns symbols and units (quantities) to common physical properties.
module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Language.Drasil.ShortHands (lM, cL, cV, lGamma, lRho)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (density, specWeight, len,
  mass, vol)
import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight)
import Data.Drasil.Units.PhysicalProperties (densityU)

physicalquants :: [DefinedQuantityDict]
physicalquants = [density, specWeight, mass, len, vol]

density, specWeight, mass, len, vol :: DefinedQuantityDict
density    = dqd CPP.density    lRho   Real densityU
specWeight = dqd CPP.specWeight lGamma Real specificWeight
mass       = dqd CPP.mass       lM     Real kilogram
len        = dqd CPP.len        cL     Real metre
vol        = dqd CPP.vol        cV     Real m_3
