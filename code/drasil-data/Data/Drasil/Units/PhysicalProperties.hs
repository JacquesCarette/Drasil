module Data.Drasil.Units.PhysicalProperties where

import Data.Drasil.SI_Units (kilogram, m_3)
import Language.Drasil (DerUChunk, new_unit, (/:))

densityU :: DerUChunk
densityU = new_unit "density"              $ kilogram /: m_3
