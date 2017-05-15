module Data.Drasil.SI_Units where
import Language.Drasil.Chunk.Concept (dcc)
import Language.Drasil.Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn(..), new_unit, (^:), (/:), (*:), makeDerU)
import Language.Drasil.Unicode (Special(Circle))
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..))
import Language.Drasil.NounPhrase

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

centigrade, joule, watt, calorie, kilowatt, pascal, newton, millimetre, 
  kilopascal, radians :: DerUChunk

centigrade = DUC 
  (UD (dcc "centigrade" (cn "centigrade") "temperature") 
      (UName (Concat [Special Circle, Atomic "C"])))
  (UShift 273.15 (kelvin ^. usymb))

joule = DUC
    (UD (dcc "joule" (cn' "joule") "energy") (UName $ Atomic "J"))
    (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb,
                      UPow (second ^. usymb) (-2)]))

calorie = DUC
  (UD (dcc "calorie" (cn' "calorie") "energy") (UName $ Atomic "cal"))
  (UScale 4.184 (joule ^. usymb))

watt = DUC
  (UD (dcc "watt" (cn' "watt") "power") (UName $ Atomic "W"))
  (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb,
                    UPow (second ^. usymb) (-3)]))

kilowatt = DUC
  (UD (dcc "kilowatt" (cn' "kilowatt") "power")
      (UName $ Concat [Atomic "k", Atomic "W"]))
  (UScale 1000 (watt ^. usymb))

pascal = DUC
  (UD (dcc "pascal" (pn' "Pascal") "pressure") (UName $ (Atomic "Pa")))
  (USynonym (UProd [(kilogram ^. usymb), (UPow (metre ^. usymb) (-1)),
                      (UPow (second ^. usymb) (-2))]))

newton = DUC
  (UD (dcc "newton" (pn' "Newton") "force") (UName $ Atomic "N"))
  (USynonym (UProd [(kilogram ^. usymb), (UPow (second ^. usymb) (-2))]))

millimetre = DUC
  (UD (dcc "millimetre" (cn' "millimetre") "length")
      (UName $ (Atomic "mm")))
  (UScale 0.0001 (metre ^. usymb))

kilopascal = DUC
  (UD (dcc "kilopascal" (cn' "kilopascal") "pressure")
      (UName $ Concat [Atomic "k", Atomic "Pa"]))
  (UScale 1000 (pascal ^. usymb))

radians = DUC
    (UD (dcc "radians" (cn' "radians") "angle") (UName $ Atomic "rad"))
    (USynonym (metre /: metre))

-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.
degree :: FundUnit --FIXME: define degree in terms of radians and pi
degree = UD (dcc "degree" (cn' "degree") "angle") (UName (Special Circle))

-- FIXME: These should probably be moved elsewhere --
    
velU, accelU, angVelU, angAccelU, momtInertU, densityU :: DerUChunk
velU         = new_unit "velocity"             $ metre /: second
accelU       = new_unit "acceleration"         $ metre /: s_2

angVelU      = new_unit "angular velocity"     $ radians /: second
angAccelU    = new_unit "angular acceleration" $ radians /: s_2
momtInertU   = new_unit "moment of inertia"    $ kilogram *: m_2
densityU     = new_unit "density"              $ kilogram /: m_3

impulseU, springConstU, torqueU :: DerUChunk
impulseU     = new_unit "impulse"              $ newton *: second
springConstU = new_unit "spring constant"      $ newton /: metre
torqueU      = new_unit "torque"               $ newton *: metre

-- Should we allow multiple different unit names for the same units?
momentOfForceU, stiffnessU :: DerUChunk
momentOfForceU = new_unit "moment of force"    $ newton *: metre
stiffnessU     = new_unit "stiffness"          $ newton /: metre 

gravConstU :: DerUChunk
gravConstU = makeDerU (dcc "gravConstU" (cn "gravitational constant")
  "universal gravitational constant") $
   USynonym (UDiv (m_3 ^. usymb) (UProd [kilogram ^. usymb, s_2 ^. usymb]))

------
specificE :: DerUChunk
specificE = makeDerU (dcc "specificE" (cnIES "specific energy") 
  "energy per unit mass") $ USynonym (joule /: kilogram)

specific_weight :: DerUChunk
specific_weight = makeDerU (dcc "specific_weight" (cn' "specific weight")
  "weight per unit volume") $
  USynonym (UDiv (newton ^. usymb) (UPow (metre ^. usymb) (3)))