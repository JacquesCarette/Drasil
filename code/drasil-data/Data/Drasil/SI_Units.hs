module Data.Drasil.SI_Units where

import Language.Drasil
import Language.Drasil.ShortHands (cOmega)

fundamentals :: [UnitDefn]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [UnitDefn]
derived = [becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber]

siUnits :: [UnitDefn]
siUnits = map unitWrapper fundamentals ++ map unitWrapper derived

------------- Fundamental SI Units ---------------------------------------------

metre, kilogram, second, kelvin, mole, ampere, candela :: UnitDefn
metre    = fund "metre"    "length"               "m"
kilogram = fund "kilogram" "mass"                 "kg"
second   = fund "second"   "time"                 "s"
kelvin   = fund "kelvin"   "temperature"          "K"
mole     = fund "mole"     "amount of substance"  "mol"
ampere   = fund "ampere"   "electric current"     "A"
candela  = fund "candela"  "luminous intensity"   "cd"

------------- Commonly defined units -------------------------------------------

degree :: UnitDefn --FIXME: define degree in terms of radians and pi
-- degree = UD (dcc "degree" (cn' "degree") "angle") (BaseSI (US [(Special Circle,1)])) ["degree"]
degree = fund' "degree" "angle" (Special Circle)

-- Some of these units are easiest to define via others less common names, 
-- which we define first.
s_2 :: UnitDefn
s_2 = newUnit "seconds squared" $ second ^: 2

m_2, m_3 :: UnitDefn
m_2 = newUnit "square metres"   $ metre ^: 2
m_3 = newUnit "cubic metres"    $ metre ^: 3

-- And now for the ones with 'common' names

becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber :: UnitDefn

becquerel = derCUC' "becquerel" 
  "becquerel" "activity" (Label "Bq") --of a Radionuclide
  (second ^: (-1))
  
calorie = derUC "calorie" 
  "calorie" "energy" (Label "cal") (scale 4.184 joule)

centigrade = derUC "centigrade" 
  "centigrade" "temperature" (Concat [Special Circle, Label "C"])
  (shift 273.15 kelvin)

coulomb = derCUC' "coulomb" 
  "coulomb" "electric charge" (Label "C") (ampere *: second)

farad = derCUC' "farad" 
  "farad" "capacitance" (Label "F") (coulomb /: volt)

gray = derCUC' "gray" 
  "gray" "absorbed dose" (Label "Gy") (joule /: kilogram)
  
henry = derCUC'' "henry" 
  (cnIES "henry") "inductance" (Label "H") (weber /: ampere)
  
hertz = derCUC "hertz" 
  "hertz" "frequency" (Label "Hz") (second ^: (-1))

joule = derCUC' "joule" 
  "joule" "energy" (Label "J") 
 (kilogram *$ (m_2 *$ (second ^: (-2))))

katal = derCUC' "katal" 
  "katal" "catalytic activity" (Label "kat") (mole /: second)

kilopascal = derUC' "kilopascal" 
  "kilopascal" "pressure"
  (Concat [Label "k", Label "Pa"]) (scale 1000 pascal)

kilowatt = derUC' "kilowatt" 
  "kilowatt" "power" (Concat [Label "k", Label "W"]) (scale 1000 watt)
  
litre = derUC' "litre"
  "litre" "volume" (Label "L") (scale (1/1000) m_3)

lumen = derCUC' "lumen" 
  "lumen" "luminous flux" (Label "lm") (candela *: steradian)

lux = derCUC "lux" 
  "lux" "illuminance" (Label "lx") (lumen /: m_2)

millimetre = derUC' "millimetre"
  "millimetre" "length" (Label "mm") (scale 0.0001 metre)

newton = derCUC' "newton"
  "newton" "force" (Label "N") (kilogram *$ (second ^: (-2)))
  
ohm = derCUC' "ohm"
  "ohm" "resistance" cOmega (volt /: ampere)
  
pascal = derCUC' "pascal" 
  "pascal" "pressure" (Label "Pa")
  (kilogram /$ (metre *$ (second ^: 2)))
  
radian = derCUC' "radian" 
  "radian" "angle" (Label "rad") (metre /: metre)
            
siemens = derCUC "siemens" 
  "siemens" "conductance" (Label "S") (ohm ^: (-1))
  
sievert = derCUC' "sievert" 
  "sievert" "dose equivalent" (Label "Sv")
  (joule /: kilogram)
            
steradian = derCUC' "steradian" 
  "steradian" "solid angle" (Label "sr") (m_2 /: m_2 )
  
tesla = derCUC "tesla"
  "tesla" "magnetic flux density" (Label "T") (weber /: m_2)

volt = derCUC' "volt" 
  "volt" "voltage" (Label "V") (watt /: ampere)

watt = derCUC' "watt" "watt" "power" (Label "W")
  (kilogram *$ (m_2 *$ (second ^: (-3))))
          
weber = derCUC' "weber"
  "weber" "magnetic flux" (Label "Wb") (volt *: second)
  
specificE :: UnitDefn
specificE = makeDerU (dcc "specificE" (cnIES "specific energy") 
  "energy per unit mass") (joule /: kilogram)

specificWeight :: UnitDefn
specificWeight = makeDerU (dcc "specificWeight" (cn' "specific weight")
  "weight per unit volume") (newton *$ (metre ^: (-3)))
  
-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
