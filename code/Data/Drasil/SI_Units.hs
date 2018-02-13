module Data.Drasil.SI_Units where
import Language.Drasil.Chunk.Concept (dcc)
import Language.Drasil.Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn, new_unit, (^:), (/:), (*:), makeDerU, shift, scale,
  derUC, derUC', derUC'', unitWrapper)
import Language.Drasil.Unicode (Special(Circle), Greek(Omega))
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..))
import Language.Drasil.NounPhrase

import Control.Lens ((^.))

fundamentals :: [FundUnit]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [DerUChunk]
derived = [becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber]

si_units :: [UnitDefn]
si_units = map unitWrapper fundamentals ++ map unitWrapper derived

------------- Fundamental SI Units ---------------------------------------------

fund :: String -> String -> String -> FundUnit
--This is one case where I don't consider having "id" and "term" equal 
-- a bad thing.
fund nam desc sym = UD (dcc nam (cn' nam) desc) (UName $ Atomic sym)

metre, kilogram, second, kelvin, mole, ampere, candela :: FundUnit
metre    = fund "metre"    "length"               "m"
kilogram = fund "kilogram" "mass"                 "kg"
second   = fund "second"   "time"                 "s"
kelvin   = fund "kelvin"   "temperature"          "K"
mole     = fund "mole"     "amount of substance"  "mol"
ampere   = fund "ampere"   "electric current"     "A"
candela  = fund "candela"  "luminous intensity"   "cd"

------------- Commonly defined units -------------------------------------------

-- Some of these units are easiest to define via others less common names, 
-- which we define first.
s_2 :: DerUChunk
s_2 = new_unit "seconds squared" $ second ^: 2

m_2, m_3 :: DerUChunk
m_2 = new_unit "square metres"   $ metre ^: 2
m_3 = new_unit "cubic metres"    $ metre ^: 3

-- And now for the ones with 'common' names

becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber :: DerUChunk

becquerel = derUC' "becquerel" 
  "becquerel" "activity" (Atomic "Bq") --of a Radionuclide
  (USynonym (second ^: (-1)))
  
calorie = derUC' "calorie" 
  "calorie" "energy" (Atomic "cal") (scale 4.184 joule)

centigrade = derUC "centigrade" 
  "centigrade" "temperature" ((Concat [Special Circle, Atomic "C"]))
  (shift 273.15 kelvin)

coulomb = derUC' "coulomb" 
  "coulomb" "electric charge" (Atomic "C") (USynonym (ampere *: second))

farad = derUC' "farad" 
  "farad" "capacitance" (Atomic "F") (USynonym (coulomb /: volt))

gray = derUC' "gray" 
  "gray" "absorbed dose" (Atomic "Gy") (USynonym (joule /: kilogram))
  
henry = derUC'' "henry" 
  (cnIES "henry") "inductance" (Atomic "H") (USynonym (weber /: ampere))
  
hertz = derUC "hertz" 
  "hertz" "frequency" (Atomic "Hz") (USynonym (second ^: (-1)))

joule = derUC' "joule" 
  "joule" "energy" (Atomic "J") 
  (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb, (second ^: (-2))]))

katal = derUC' "katal" 
  "katal" "catalytic activity" (Atomic "kat") (USynonym (mole /: second))

kilopascal = derUC' "kilopascal" 
  "kilopascal" "pressure"
  (Concat [Atomic "k", Atomic "Pa"]) (scale 1000 pascal)

kilowatt = derUC' "kilowatt" 
  "kilowatt" "power" (Concat [Atomic "k", Atomic "W"]) (scale 1000 watt)
  
litre = derUC' "litre"
  "litre" "volume" (Atomic "L") (scale (1/1000) m_3)

lumen = derUC' "lumen" 
  "lumen" "luminous flux" (Atomic "lm") (USynonym (candela *: steradian))

lux = derUC "lux" 
  "lux" "illuminance" (Atomic "lx") (USynonym (lumen /: m_2))

millimetre = derUC' "millimetre"
  "millimetre" "length" (Atomic "mm") (scale 0.0001 metre)

newton = derUC' "newton"
  "newton" "force" (Atomic "N")
  (USynonym (UProd [(kilogram ^. usymb), (second ^: (-2))]))
  
ohm = derUC' "ohm"
  "ohm" "resistance" (Greek Omega) (USynonym (volt /: ampere))
  
pascal = derUC' "pascal" 
  "pascal" "pressure" (Atomic "Pa")
  (USynonym (UProd [(kilogram ^. usymb), (metre ^: (-1)), (second ^: (-2))]))
  
radian = derUC' "radian" 
  "radian" "angle" (Atomic "rad") (USynonym (metre /: metre))
            
siemens = derUC "siemens" 
  "siemens" "conductance" (Atomic "S") (USynonym (ohm ^: (-1)))
  
sievert = derUC' "sievert" 
  "sievert" "dose equivalent" (Atomic "Sv")
  (USynonym (joule /: kilogram))
            
steradian = derUC' "steradian" 
  "steradian" "solid angle" (Atomic "sr") (USynonym (m_2 /: m_2 ))
  
tesla = derUC "tesla"
  "tesla" "magnetic flux density" (Atomic "T") (USynonym (weber /: m_2))

volt = derUC' "volt" 
  "volt" "voltage" (Atomic "V") (USynonym (watt /: ampere))

watt = derUC' "watt" "watt" "power" (Atomic "W")
  (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb, (second ^: (-3))]))
          
weber = derUC' "weber"
  "weber" "magnetic flux" (Atomic "Wb") (USynonym (volt *: second))
  
degree :: FundUnit --FIXME: define degree in terms of radians and pi
degree = UD (dcc "degree" (cn' "degree") "angle") (UName (Special Circle))

specificE :: DerUChunk
specificE = makeDerU (dcc "specificE" (cnIES "specific energy") 
  "energy per unit mass") $ USynonym (joule /: kilogram)

specific_weight :: DerUChunk
specific_weight = makeDerU (dcc "specific_weight" (cn' "specific weight")
  "weight per unit volume") $
  USynonym (UDiv (newton ^. usymb) (metre ^: 3))
  
-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
