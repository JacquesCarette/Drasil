{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 

module SI_Units where
import Chunk (VarChunk(..))
import UnitalChunk (UnitalChunk(..))
import ASTInternal (Expr(..))
import Unit (Unit(..))
import Unicode (Circle(..))
import Spec (Spec(..))

fundamentals :: [UnitalChunk]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [UnitalChunk]
derived = [centigrade, joule, watt, calorie]

si_units :: [UnitalChunk]
si_units = fundamentals ++ derived

-- -- Fundamental SI Units --------------------------------------------------------
fund :: String -> String -> String -> UnitalChunk
fund nam desc sym = UC (VC {vname = nam, vdesc = desc , vsymb = S sym}) Fundamental

metre, kilogram, second, kelvin, mole, ampere, candela :: UnitalChunk
metre = fund "Metre" "length (metre)" "m"
kilogram = fund "Kilogram" "mass (kilogram)" "kg"
second = fund "Second" "time (second)" "s"
kelvin = fund "Kelvin" "temperature (kelvin)" "K"
mole = fund "Mole" "amount of substance (mole)" "mol"
ampere = fund "Ampere" "electric current (ampere)" "A"
candela = fund "Candela" "luminous intensity (candela)" "cd"

-- ------- END FUNDAMENTALS -------------------------------------------------------

centigrade, joule, watt, calorie, kilowatt :: UnitalChunk

centigrade = UC (VC "Centigrade" "temperature (centigrade)" (U Circle :+: S "C"))
  (Derived (C kelvin :- (Dbl 273.15)))

joule = UC (VC "Joule" "energy (joule)" (S "J"))
    (Derived ((C kilogram :* (C metre :^ (Int 2))) :/ (C second :^ (Int 2))))

calorie = UC (VC "Calorie" "energy (calorie)" (S "cal"))
  (Derived ((Dbl 4.184) :* (C joule)))

watt = UC (VC "Watt" "power (watt)" (S "W"))
  (Derived ((C kilogram :* (C metre :^ (Int 2))) :/ (C second :^ (Int 3))))

kilowatt = UC (VC "Kilowatt" "power (kilowatt)" (S "kW"))
  (Derived ((Int 1000) :* C watt))

