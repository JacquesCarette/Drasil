-- | Defines units for use in Drasil. Often used in a unital-related chunk that has an associated symbol.
module Data.Drasil.SI_Units where

import Language.Drasil
import Language.Drasil.Display (Symbol(Special))
import Language.Drasil.ShortHands (cOmega)
import Drasil.Database (mkUid)

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
metre    = baseUnit "metre"    "length"               (label "m")
kilogram = baseUnit "kilogram" "mass"                 (label "kg")
second   = baseUnit "second"   "time"                 (label "s")
kelvin   = baseUnit "kelvin"   "temperature"          (label "K")
mole     = baseUnit "mole"     "amount of substance"  (label "mol")
ampere   = baseUnit "ampere"   "electric current"     (label "A")
candela  = baseUnit "candela"  "luminous intensity"   (label "cd")

-- * Commonly Defined Units

degree :: UnitDefn --FIXME: define degree in terms of radians and pi
degree = baseUnit "degree" "angle" (Special Circle)

-- Some of these units are easiest to define via others less common names,
-- which we define first.
s_2 :: UnitDefn
s_2 = compoundUnit' "seconds squared" $ second ^: 2

m_2, m_3 :: UnitDefn
m_2 = compoundUnit' "square metres"   $ metre ^: 2
m_3 = compoundUnit' "cubic metres"    $ metre ^: 3

-- And now for the ones with 'common' names

becquerel, calorie, centigrade, coulomb, farad, gray, henry, hertz, joule,
  katal, kilopascal, kilowatt, litre, lumen, lux,  millimetre, newton, ohm,
  pascal, radian, siemens, sievert, steradian, tesla, volt, watt, weber :: UnitDefn

becquerel = derivedUnit "becquerel"
  (cn' "becquerel") "activity" (label "Bq") --of a Radionuclide
  (second ^: (-1))

calorie = derUC "calorie"
  "calorie" "energy" (label "cal") (scale 4.184 joule)

centigrade = derUC "centigrade"
  "centigrade" "temperature" (Special Circle <> label "C")
  (shift 273.15 kelvin)

coulomb = derivedUnit "coulomb"
  (cn' "coulomb") "electric charge" (label "C") (ampere *: second)

farad = derivedUnit "farad"
  (cn' "farad") "capacitance" (label "F") (coulomb /: volt)

gray = derivedUnit "gray"
  (cn' "gray") "absorbed dose" (label "Gy") (joule /: kilogram)

henry = derivedUnit "henry"
  (cnIES "henry") "inductance" (label "H") (weber /: ampere)

hertz = derivedUnit "hertz"
  (cn "hertz") "frequency" (label "Hz") (second ^: (-1))

joule = derivedUnit "joule"
  (cn' "joule") "energy" (label "J")
 (kilogram *$ (m_2 *$ (second ^: (-2))))

katal = derivedUnit "katal"
  (cn' "katal") "catalytic activity" (label "kat") (mole /: second)

kilopascal = derUC' "kilopascal"
  "kilopascal" "pressure"
  (label "k" <> label "Pa") (scale 1000 pascal)

kilowatt = derUC' "kilowatt"
  "kilowatt" "power" (label "k" <> label "W") (scale 1000 watt)

litre = derUC' "litre"
  "litre" "volume" (label "L") (scale (1/1000) m_3)

lumen = derivedUnit "lumen"
  (cn' "lumen") "luminous flux" (label "lm") (candela *: steradian)

lux = derivedUnit "lux"
  (cn "lux") "illuminance" (label "lx") (lumen /: m_2)

millimetre = derUC' "millimetre"
  "millimetre" "length" (label "mm") (scale 0.0001 metre)

newton = derivedUnit "newton"
  (cn' "newton") "force" (label "N") (kilogram *$ (metre *$ (second ^: (-2))))

ohm = derivedUnit "ohm"
  (cn' "ohm") "resistance" cOmega (volt /: ampere)

pascal = derivedUnit "pascal"
  (cn' "pascal") "pressure" (label "Pa")
  (kilogram /$ (metre *$ (second ^: 2)))

radian = derivedUnit "radian"
  (cn' "radian") "angle" (label "rad") (metre /: metre)

siemens = derivedUnit "siemens"
  (cn "siemens") "conductance" (label "S") (ohm ^: (-1))

sievert = derivedUnit "sievert"
  (cn' "sievert") "dose equivalent" (label "Sv")
  (joule /: kilogram)

steradian = derivedUnit "steradian"
  (cn' "steradian") "solid angle" (label "sr") (m_2 /: m_2 )

tesla = derivedUnit "tesla"
  (cn "tesla") "magnetic flux density" (label "T") (weber /: m_2)

volt = derivedUnit "volt"
  (cn' "volt") "voltage" (label "V") (watt /: ampere)

watt = derivedUnit "watt" (cn' "watt") "power" (label "W")
  (kilogram *$ (m_2 *$ (second ^: (-3))))

weber = derivedUnit "weber"
  (cn' "weber") "magnetic flux" (label "Wb") (volt *: second)

specificE :: UnitDefn
specificE = compoundUnit (cncpt''' (mkUid "specificE") (cnIES "specific energy")
  (S "energy per unit mass")) (joule /: kilogram)

specificWeight :: UnitDefn
specificWeight = compoundUnit (cncpt''' (mkUid "specificWeight") (cn' "specific weight")
  (S "weight per unit volume")) (newton *$ (metre ^: (-3)))

-- FIXME: Need to add pi
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
