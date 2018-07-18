module Data.Drasil.SI_Units where
import Language.Drasil
import Language.Drasil.Development

fundamentals :: [UnitDefn]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [UnitDefn]
derived = [becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber]

si_units :: [UnitDefn]
si_units = map unitWrapper fundamentals ++ map unitWrapper derived

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
degree = UD (dcc "degree" (cn' "degree") "angle") (US [(Special Circle,1)]) Nothing Nothing []

-- Some of these units are easiest to define via others less common names, 
-- which we define first.
s_2 :: UnitDefn
s_2 = new_unit "seconds squared" $ second ^: 2

m_2, m_3 :: UnitDefn
m_2 = new_unit "square metres"   $ metre ^: 2
m_3 = new_unit "cubic metres"    $ metre ^: 3

-- And now for the ones with 'common' names

becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber :: UnitDefn

becquerel = derCUC' "becquerel" 
  "becquerel" "activity" (Atomic "Bq") --of a Radionuclide
  (second ^: (-1))
  
calorie = derUC "calorie" 
  "calorie" "energy" (Atomic "cal") (scale 4.184 joule)

centigrade = derUC "centigrade" 
  "centigrade" "temperature" ((Concat [Special Circle, Atomic "C"]))
  (shift 273.15 kelvin)

coulomb = derCUC' "coulomb" 
  "coulomb" "electric charge" (Atomic "C") (ampere *: second)

farad = derCUC' "farad" 
  "farad" "capacitance" (Atomic "F") (coulomb /: volt)

gray = derCUC' "gray" 
  "gray" "absorbed dose" (Atomic "Gy") (joule /: kilogram)
  
henry = derCUC'' "henry" 
  (cnIES "henry") "inductance" (Atomic "H") (weber /: ampere)
  
hertz = derCUC "hertz" 
  "hertz" "frequency" (Atomic "Hz") (second ^: (-1))

joule = derCUC' "joule" 
  "joule" "energy" (Atomic "J") 
 (kilogram *$ (m_2 *$ (second ^: (-2))))

katal = derCUC' "katal" 
  "katal" "catalytic activity" (Atomic "kat") (mole /: second)

kilopascal = derUC' "kilopascal" 
  "kilopascal" "pressure"
  (Concat [Atomic "k", Atomic "Pa"]) (scale 1000 pascal)

kilowatt = derUC' "kilowatt" 
  "kilowatt" "power" (Concat [Atomic "k", Atomic "W"]) (scale 1000 watt)
  
litre = derUC' "litre"
  "litre" "volume" (Atomic "L") (scale (1/1000) m_3)

lumen = derCUC' "lumen" 
  "lumen" "luminous flux" (Atomic "lm") (candela *: steradian)

lux = derCUC "lux" 
  "lux" "illuminance" (Atomic "lx") (lumen /: m_2)

millimetre = derUC' "millimetre"
  "millimetre" "length" (Atomic "mm") (scale 0.0001 metre)

newton = derCUC' "newton"
  "newton" "force" (Atomic "N") (kilogram *$ (second ^: (-2)))
  
ohm = derCUC' "ohm"
  "ohm" "resistance" cOmega (volt /: ampere)
  
pascal = derCUC' "pascal" 
  "pascal" "pressure" (Atomic "Pa")
  (kilogram /$ (metre *$ (second ^: 2)))
  
radian = derCUC' "radian" 
  "radian" "angle" (Atomic "rad") (metre /: metre)
            
siemens = derCUC "siemens" 
  "siemens" "conductance" (Atomic "S") (ohm ^: (-1))
  
sievert = derCUC' "sievert" 
  "sievert" "dose equivalent" (Atomic "Sv")
  (joule /: kilogram)
            
steradian = derCUC' "steradian" 
  "steradian" "solid angle" (Atomic "sr") (m_2 /: m_2 )
  
tesla = derCUC "tesla"
  "tesla" "magnetic flux density" (Atomic "T") (weber /: m_2)

volt = derCUC' "volt" 
  "volt" "voltage" (Atomic "V") (watt /: ampere)

watt = derCUC' "watt" "watt" "power" (Atomic "W")
  (kilogram *$ (m_2 *$ (second ^: (-3))))
          
weber = derCUC' "weber"
  "weber" "magnetic flux" (Atomic "Wb") (volt *: second)
  
specificE :: UnitDefn
specificE = makeDerU (dcc "specificE" (cnIES "specific energy") 
  "energy per unit mass") (joule /: kilogram)

specific_weight :: UnitDefn
specific_weight = makeDerU (dcc "specific_weight" (cn' "specific weight")
  "weight per unit volume") (newton *$ (metre ^: (-3)))
  
-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
