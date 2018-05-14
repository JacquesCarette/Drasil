module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.PhysicalProperties as CPP
import Data.Drasil.Units.PhysicalProperties
import Data.Drasil.SI_Units

density, mass, len, vol :: UnitalChunk
density = ucEL CPP.density (Greek Rho_L) densityU
mass    = ucEL CPP.mass    lM            kilogram
len     = ucEL CPP.len     cL            metre
vol     = ucEL CPP.vol     cV            m_3
