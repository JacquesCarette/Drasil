module Data.Drasil.Units.Chemistry where

import Data.Drasil.SI_Units (mole, kilogram)


densityU :: DerUChunk

molarityU     = new_unit "molarity"              $ mole /: litre
molalityU     = new_unit "molality"              $ mole /: kilogram