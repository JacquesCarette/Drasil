module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.PhysicalProperties as CPP

--FIXME: Space hacks
mass, length :: ConVar

mass = cv CPP.mass lM Rational
length = cv CPP.length cL Rational

density, vol :: UnitalChunk
density = uc CPP.density (Greek Rho_L) densityU
vol = uc CPP.vol cV m_3