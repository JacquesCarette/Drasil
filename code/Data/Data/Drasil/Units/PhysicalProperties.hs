module Data.Drasil.Units.PhysicalProperties where

import Language.Drasil
import Data.Drasil.SI_Units

densityU :: DerUChunk
densityU = new_unit "density"              $ kilogram /: m_3
