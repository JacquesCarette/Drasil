module Data.Drasil.Units.Chemistry where

import Language.Drasil
import Data.Drasil.SI_Units


densityU :: DerUChunk

molarityU     = new_unit "molarity"              $ mole /: litre
molalityU     = new_unit "molality"              $ mole /: kilogram