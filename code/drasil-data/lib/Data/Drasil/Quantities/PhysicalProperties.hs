-- | Assigns symbols and units (quantities) to common physical properties.
module Data.Drasil.Quantities.PhysicalProperties where

import Drasil.Database (mkUid)
import Language.Drasil
import Language.Drasil.ShortHands (lM, cL, cV, lGamma, lRho)

import Data.Drasil.SI_Units (kilogram, metre, m_3, specificWeight)
import Data.Drasil.Units.PhysicalProperties (densityU)

physicalquants :: [DefinedQuantityDict]
physicalquants = [density, specWeight, mass, len, vol]

density, specWeight, mass, len, vol :: DefinedQuantityDict
density    = quant (mkUid "density")    (cnIES "density"      ) (S "the mass per unit volume")
  lRho   Real densityU
specWeight = quant (mkUid "specWeight") (cn' "specific weight") (S "the weight per unit volume")
  lGamma Real specificWeight
mass       = quant (mkUid "mass")       (cn''' "mass"         ) (S "the quantity of matter in a body")
  lM     Real kilogram
len        = quant (mkUid "length")     (cn' "length"         ) (S ("the straight-line distance between two points along an object, " ++
                                                                    "typically used to represent the size of an object from one end to the other"))
  cL     Real metre
vol        = quant (mkUid "volume")     (cn' "volume"         ) (S "the amount of space that a substance or object occupies")
  cV     Real m_3
