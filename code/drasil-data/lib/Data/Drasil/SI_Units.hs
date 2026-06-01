-- | Defines units for use in Drasil. Often used in a unital-related chunk that has an associated symbol.
module Data.Drasil.SI_Units where

import Language.Drasil
import Language.Drasil.Display
import Language.Drasil.ShortHands (cOmega)

-- * Lists of Units

fundamentals :: [UnitDefn]
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

derived :: [UnitDefn]
derived = [becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber]

common :: [UnitDefn]
common = [s_2, m_2, m_3]

siUnits :: [UnitDefn]
siUnits = fundamentals ++ derived ++ common

-- * Fundamental SI Units

metre, kilogram, second, kelvin, mole, ampere, candela :: UnitDefn
metre    = fund "metre"    "length"               "m"
kilogram = fund "kilogram" "mass"                 "kg"
second   = fund "second"   "time"                 "s"
kelvin   = fund "kelvin"   "temperature"          "K"
mole     = fund "mole"     "amount of substance"  "mol"
ampere   = fund "ampere"   "electric current"     "A"
candela  = fund "candela"  "luminous intensity"   "cd"

-- * Commonly Defined Units

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
  "becquerel" "activity" (label "Bq") --of a Radionuclide
  (second ^: (-1))

calorie = derUC "calorie"
  "calorie" "energy" (label "cal") (scale 4.184 joule)

centigrade = derUC "centigrade"
  "centigrade" "temperature" (Concat [Special Circle, label "C"])
  (shift 273.15 kelvin)

coulomb = derCUC' "coulomb"
  "coulomb" "electric charge" (label "C") (ampere *: second)

farad = derCUC' "farad"
  "farad" "capacitance" (label "F") (coulomb /: volt)

gray = derCUC' "gray"
  "gray" "absorbed dose" (label "Gy") (joule /: kilogram)

henry = derCUC'' "henry"
  (cnIES "henry") "inductance" (label "H") (weber /: ampere)

hertz = derCUC "hertz"
  "hertz" "frequency" (label "Hz") (second ^: (-1))

joule = derCUC' "joule"
  "joule" "energy" (label "J")
 (kilogram *$ (m_2 *$ (second ^: (-2))))

katal = derCUC' "katal"
  "katal" "catalytic activity" (label "kat") (mole /: second)

kilopascal = derUC' "kilopascal"
  "kilopascal" "pressure"
  (Concat [label "k", label "Pa"]) (scale 1000 pascal)

kilowatt = derUC' "kilowatt"
  "kilowatt" "power" (Concat [label "k", label "W"]) (scale 1000 watt)

litre = derUC' "litre"
  "litre" "volume" (label "L") (scale (1/1000) m_3)

lumen = derCUC' "lumen"
  "lumen" "luminous flux" (label "lm") (candela *: steradian)

lux = derCUC "lux"
  "lux" "illuminance" (label "lx") (lumen /: m_2)

millimetre = derUC' "millimetre"
  "millimetre" "length" (label "mm") (scale 0.0001 metre)

newton = derCUC' "newton"
  "newton" "force" (label "N") (kilogram *$ (second ^: (-2)))

ohm = derCUC' "ohm"
  "ohm" "resistance" cOmega (volt /: ampere)

pascal = derCUC' "pascal"
  "pascal" "pressure" (label "Pa")
  (kilogram /$ (metre *$ (second ^: 2)))

radian = derCUC' "radian"
  "radian" "angle" (label "rad") (metre /: metre)

siemens = derCUC "siemens"
  "siemens" "conductance" (label "S") (ohm ^: (-1))

sievert = derCUC' "sievert"
  "sievert" "dose equivalent" (label "Sv")
  (joule /: kilogram)

steradian = derCUC' "steradian"
  "steradian" "solid angle" (label "sr") (m_2 /: m_2 )

tesla = derCUC "tesla"
  "tesla" "magnetic flux density" (label "T") (weber /: m_2)

volt = derCUC' "volt"
  "volt" "voltage" (label "V") (watt /: ampere)

watt = derCUC' "watt" "watt" "power" (label "W")
  (kilogram *$ (m_2 *$ (second ^: (-3))))

weber = derCUC' "weber"
  "weber" "magnetic flux" (label "Wb") (volt *: second)

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
