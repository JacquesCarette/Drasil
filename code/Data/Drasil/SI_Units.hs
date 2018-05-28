module Data.Drasil.SI_Units where
import Language.Drasil

-- These are not normally all exported, but need them here. Should probably create
-- some kind of Language.Drasil.Development module... FIXME
import Language.Drasil.UnitLang(UDefn(..))
import Language.Drasil.Unit (UnitDefn(..),
  UnitDefn, new_unit, (^:), (/:), (*:), makeDerU, shift, scale,
  derUC, derUC', derUC'', unitWrapper, fshift, fscale)

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
degree = UD (dcc "degree" (cn' "degree") "angle") (US [(Special Circle,1)]) Nothing

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

becquerel = derUC' "becquerel" 
  "becquerel" "activity" (Atomic "Bq") --of a Radionuclide
  (FUSynonym (second ^: (-1)))
  
calorie = derUC' "calorie" 
  "calorie" "energy" (Atomic "cal") (scale 4.184 joule)

centigrade = derUC "centigrade" 
  "centigrade" "temperature" ((Concat [Special Circle, Atomic "C"]))
  (fshift 273.15 kelvin)

coulomb = derUC' "coulomb" 
  "coulomb" "electric charge" (Atomic "C") (FUSynonym (ampere *: second))

farad = derUC' "farad" 
  "farad" "capacitance" (Atomic "F") (FUSynonym (coulomb /: volt))

gray = derUC' "gray" 
  "gray" "absorbed dose" (Atomic "Gy") (FUSynonym (joule /: kilogram))
  
henry = derUC'' "henry" 
  (cnIES "henry") "inductance" (Atomic "H") (FUSynonym (weber /: ampere))
  
hertz = derUC "hertz" 
  "hertz" "frequency" (Atomic "Hz") (FUSynonym (second ^: (-1)))

joule = derUC' "joule" 
  "joule" "energy" (Atomic "J") 
  (FUSynonym $ kilogram *$ (m_2 *$ (second ^: (-2))))

katal = derUC' "katal" 
  "katal" "catalytic activity" (Atomic "kat") (FUSynonym (mole /: second))

kilopascal = derUC' "kilopascal" 
  "kilopascal" "pressure"
  (Concat [Atomic "k", Atomic "Pa"]) (fscale 1000 pascal)

kilowatt = derUC' "kilowatt" 
  "kilowatt" "power" (Concat [Atomic "k", Atomic "W"]) (fscale 1000 watt)
  
litre = derUC' "litre"
  "litre" "volume" (Atomic "L") (fscale (1/1000) m_3)

lumen = derUC' "lumen" 
  "lumen" "luminous flux" (Atomic "lm") (FUSynonym (candela *: steradian))

lux = derUC "lux" 
  "lux" "illuminance" (Atomic "lx") (FUSynonym (lumen /: m_2))

millimetre = derUC' "millimetre"
  "millimetre" "length" (Atomic "mm") (fscale 0.0001 metre)

newton = derUC' "newton"
  "newton" "force" (Atomic "N") (FUSynonym $ kilogram *$ (second ^: (-2)))
  
ohm = derUC' "ohm"
  "ohm" "resistance" (Greek Omega) (FUSynonym (volt /: ampere))
  
pascal = derUC' "pascal" 
  "pascal" "pressure" (Atomic "Pa")
  (FUSynonym $ kilogram /$ (metre *$ (second ^: 2)))
  
radian = derUC' "radian" 
  "radian" "angle" (Atomic "rad") (FUSynonym (metre /: metre))
            
siemens = derUC "siemens" 
  "siemens" "conductance" (Atomic "S") (FUSynonym (ohm ^: (-1)))
  
sievert = derUC' "sievert" 
  "sievert" "dose equivalent" (Atomic "Sv")
  (FUSynonym (joule /: kilogram))
            
steradian = derUC' "steradian" 
  "steradian" "solid angle" (Atomic "sr") (FUSynonym (m_2 /: m_2 ))
  
tesla = derUC "tesla"
  "tesla" "magnetic flux density" (Atomic "T") (FUSynonym (weber /: m_2))

volt = derUC' "volt" 
  "volt" "voltage" (Atomic "V") (FUSynonym (watt /: ampere))

watt = derUC' "watt" "watt" "power" (Atomic "W")
  (FUSynonym $ kilogram *$ (m_2 *$ (second ^: (-3))))
          
weber = derUC' "weber"
  "weber" "magnetic flux" (Atomic "Wb") (FUSynonym (volt *: second))
  
specificE :: UnitDefn
specificE = makeDerU (dcc "specificE" (cnIES "specific energy") 
  "energy per unit mass") $ USynonym (joule /: kilogram)

specific_weight :: UnitDefn
specific_weight = makeDerU (dcc "specific_weight" (cn' "specific weight")
  "weight per unit volume") $
  USynonym $ newton *$ (metre ^: (-3))
  
-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
