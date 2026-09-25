-- | Defines units to describe physical properties.
module Data.Drasil.Units.PhysicalProperties where

import Data.Drasil.SI_Units (kilogram, m_3)
import Language.Drasil (UnitDefn, compoundUnit', (/:))

densityU :: UnitDefn
densityU = compoundUnit' "density"              $ kilogram /: m_3
