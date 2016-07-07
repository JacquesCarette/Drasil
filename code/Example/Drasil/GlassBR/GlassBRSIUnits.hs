{-# OPTIONS -Wall #-} 
module Example.Drasil.GlassBR.GlassBRSIUnits where
import Language.Drasil

import Control.Lens ((^.))

fundamentals :: [FundUnit]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [DerUChunk]
derived = [centigrade, joule, watt, calorie, kilowatt, pascal, newton, millimetre, kilopascal]

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
unitless = fund "Unitless" "-"                    "unitless"

------------- END FUNDAMENTALS -------------------------------------------------

centigrade, joule, watt, calorie, kilowatt, pascal, newton, millimetre, kilopascal:: DerUChunk 

centigrade = DUC 
  (UD (CC "Centigrade" (S "temperature")) 
      (UName (Concat [Special Circle, Atomic "C"])))
  (UShift 273.15 (kelvin ^. unit))

joule = DUC
    (UD (CC "Joule" (S "energy")) (UName $ Atomic "J"))
    (USynonym (UProd [kilogram ^. unit, UPow (metre ^. unit) 2,
                      UPow (second ^. unit) (-2)]))

calorie = DUC
  (UD (CC "Calorie" (S "energy")) (UName $ Atomic "cal"))
  (UScale 4.184 (joule ^. unit))

watt = DUC
  (UD (CC "Watt" (S "power")) (UName $ Atomic "W"))
  (USynonym (UProd [kilogram ^. unit, UPow (metre ^. unit)2, 
                    UPow (second ^. unit) (-3)]))

kilowatt = DUC
  (UD (CC "Kilowatt" (S "power"))
      (UName $ Concat [Atomic "k", Atomic "W"]))
  (UScale 1000 (watt ^. unit))

pascal = DUC
  (UD (CC "Pascal" (S "pressure")) (UName $ (Atomic "Pa")))
  (USynonym (UProd [(kilogram ^. unit), (UPow (metre ^. unit) (-1)),
                      (UPow (second ^. unit) (-2))]))

newton = DUC
  (UD (CC "Newton" (S "force")) (UName $ Atomic "N"))
  (USynonym (UProd [(kilogram ^. unit), (UPow (second ^. unit) (-2))]))

millimetre = DUC
  (UD (CC "Millimetre" (S "length"))
      (UName $ (Atomic "mm")))
  (UScale 0.0001 (metre ^. unit))

kilopascal = DUC
  (UD (CC "Kilopascal" (S "pressure"))
      (UName $ Concat [Atomic "k", Atomic "Pa"]))
  (UScale 1000 (pascal ^. unit))





