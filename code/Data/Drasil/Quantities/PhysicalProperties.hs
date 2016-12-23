module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.PhysicalProperties as CPP

--FIXME: Space hacks
mass :: ConVar
mass = cv CPP.mass lM Rational

length :: ConVar
length = cv CPP.length cL Rational