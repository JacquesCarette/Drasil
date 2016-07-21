module Language.Drasil.SI_Units where
import Language.Drasil.Chunk (ConceptChunk(..))
import Language.Drasil.Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn(..))
import Language.Drasil.Unicode (Special(Circle))
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..),Sentence(..))

import Control.Lens ((^.))

fundamentals :: [FundUnit]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [DerUChunk]
derived = [centigrade, joule, watt, calorie, kilowatt, pascal, newton, 
  millimetre, kilopascal, radians]

si_units :: [UnitDefn]
si_units = map UU fundamentals ++ map UU derived

------------- Fundamental SI Units ---------------------------------------------
fund :: String -> String -> String -> FundUnit
fund nam desc sym = UD (CC nam (S desc)) (UName $ Atomic sym)

metre, kilogram, second, kelvin, mole, ampere, candela :: FundUnit
metre    = fund "Metre"    "length"               "m"
kilogram = fund "Kilogram" "mass"                 "kg"
second   = fund "Second"   "time"                 "s"
kelvin   = fund "Kelvin"   "temperature"          "K"
mole     = fund "Mole"     "amount of substance"  "mol"
ampere   = fund "Ampere"   "electric current"     "A"
candela  = fund "Candela"  "luminous intensity"   "cd"

------------- END FUNDAMENTALS -------------------------------------------------

centigrade, joule, watt, calorie, kilowatt, pascal, newton, millimetre, 
  kilopascal:: DerUChunk 

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

radians = DUC
    (UD (CC "Radians" (S "angle")) (UName $ Atomic "rad"))
    (USynonym (UProd [metre ^. unit, UPow (metre ^. unit) (-1)]))
