-- | Defines units to describe physical properties.
module Data.Drasil.Units.PhysicalProperties where

import Data.Drasil.SI_Units (kilogram, m_3)
import Language.Drasil (UnitDefn, newUnit, (/:))

physicalPropertyUnits :: [UnitDefn]
physicalPropertyUnits = [densityU]

densityU :: UnitDefn
densityU = newUnit "densityU" $ kilogram /: m_3
