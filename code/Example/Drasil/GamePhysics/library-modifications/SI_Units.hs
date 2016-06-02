module Language.Drasil.SI_Units where
import Language.Drasil.Chunk (ConceptChunk(..))
import Language.Drasil.Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn(..))
import Language.Drasil.Unicode (Circle(..))
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..),Sentence(..))

import Control.Lens ((^.))

fundamentals :: [FundUnit]
fundamentals = [metre, kilogram, second]

derived :: [DerUChunk]
derived = [newton, radians]

si_units :: [UnitDefn]
si_units = map UU fundamentals ++ map UU derived

------------- Fundamental SI Units ---------------------------------------------
fund :: String -> String -> String -> FundUnit
fund nam desc sym = UD (CC nam (S desc)) (UName $ Atomic sym)

metre, kilogram, second, kelvin, mole, ampere, candela, unitless :: FundUnit
metre    = fund "Metre"    "length"               "m"
kilogram = fund "Kilogram" "mass"                 "kg"
second   = fund "Second"   "time"                 "s"
kelvin   = fund "Kelvin"   "temperature"          "K"
mole     = fund "Mole"     "amount of substance"  "mol"
ampere   = fund "Ampere"   "electric current"     "A"
candela  = fund "Candela"  "luminous intensity"   "cd"

unitless = fund "Unitless" "unitless"             "unitless"

-- NOTE: unitless added under SI units

------------- END FUNDAMENTALS -------------------------------------------------

newton, radians :: DerUChunk

newton = DUC
  (UD (CC "Newton" (S "force")) (UName $ Atomic "N"))
  (USynonym (UProd [kilogram ^. unit, metre ^. unit,
                    UPow (second ^. unit) (-2)]))

radians = DUC
  (UD (CC "Radians" (S "angle")) (UName $ Atomic "rad"))
  (USynonym (UProd [metre ^. unit, UPow (metre ^. unit) (-1)]))
