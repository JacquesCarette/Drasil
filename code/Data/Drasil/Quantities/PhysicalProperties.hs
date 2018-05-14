module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.PhysicalProperties as CPP
import Data.Drasil.Units.PhysicalProperties
import Data.Drasil.SI_Units

density, mass, len, vol :: UnitalChunk
density = uc CPP.density (Greek Rho_L) densityU []
mass    = uc CPP.mass    lM            kilogram []
len     = uc CPP.len     cL            metre []
vol     = uc CPP.vol     cV            m_3 []
