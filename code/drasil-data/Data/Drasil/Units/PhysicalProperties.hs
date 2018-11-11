module Data.Drasil.Units.PhysicalProperties where

import Data.Drasil.SI_Units (kilogram, m_3)
import Language.Drasil.Development (UnitDefn, new_unit, (/:))

densityU :: UnitDefn
densityU = new_unit "density"              $ kilogram /: m_3
