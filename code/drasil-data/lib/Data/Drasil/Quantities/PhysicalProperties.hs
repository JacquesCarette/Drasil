-- | Assigns symbols and units (quantities) to common physical properties.
module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Drasil.Database (mkUid)
import Language.Drasil.ShortHands (lM, cL, cV, lGamma, lRho)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (len, mass, vol)
import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight)
import Data.Drasil.Units.PhysicalProperties (densityU)

physicalquants :: [DefinedQuantityDict]
physicalquants = [density, specWeight, mass, len, vol]

density, specWeight, mass, len, vol :: DefinedQuantityDict
density    = quant (mkUid "density") (cnIES "density")
  (S "the mass per unit volume") lRho Real densityU
specWeight = quant (mkUid "specWeight") (cn' "specific weight")
  (S "the weight per unit volume") lGamma Real specificWeight
mass       = dqd CPP.mass       lM     Real kilogram
len        = dqd CPP.len        cL     Real metre
vol        = dqd CPP.vol        cV     Real m_3
