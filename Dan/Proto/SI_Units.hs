{-# OPTIONS -Wall #-} 
module SI_Units where
import Chunk (ConceptChunk(..))
import Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn(..))
import Unicode (Circle(..))
import Symbol
import Spec (USymb(..),Sentence(..))

import Control.Lens ((^.))

fundamentals :: [FundUnit]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [DerUChunk]
derived = [centigrade, joule, watt, calorie, kilowatt]

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

centigrade, joule, watt, calorie, kilowatt :: DerUChunk 

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
