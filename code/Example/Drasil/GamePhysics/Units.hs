module Drasil.GamePhysics.Units where

import Language.Drasil
import Data.Drasil.SI_Units

----- Table of Units -----

cpSIUnits :: [UnitDefn]
cpSIUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]
