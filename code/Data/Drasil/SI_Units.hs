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
metre    = fund "Metre"    "length"               "m"
kilogram = fund "Kilogram" "mass"                 "kg"
second   = fund "Second"   "time"                 "s"
kelvin   = fund "Kelvin"   "temperature"          "K"
mole     = fund "Mole"     "amount of substance"  "mol"
ampere   = fund "Ampere"   "electric current"     "A"
candela  = fund "Candela"  "luminous intensity"   "cd"

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
  (UD (dcc "centigrade" (cn "Centigrade") "temperature") 
      (UName (Concat [Special Circle, Atomic "C"])))
  (UShift 273.15 (kelvin ^. usymb))

joule = DUC
    (UD (dcc "joule" (cn' "Joule") "energy") (UName $ Atomic "J"))
    (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb,
                      UPow (second ^. usymb) (-2)]))

calorie = DUC
  (UD (dcc "calorie" (cn' "Calorie") "energy") (UName $ Atomic "cal"))
  (UScale 4.184 (joule ^. usymb))

watt = DUC
  (UD (dcc "watt" (cn' "Watt") "power") (UName $ Atomic "W"))
  (USynonym (UProd [kilogram ^. usymb, m_2 ^. usymb,
                    UPow (second ^. usymb) (-3)]))

kilowatt = DUC
  (UD (dcc "kilowatt" (cn' "Kilowatt") "power")
      (UName $ Concat [Atomic "k", Atomic "W"]))
  (UScale 1000 (watt ^. usymb))

pascal = DUC
  (UD (dcc "pascal" (cn' "Pascal") "pressure") (UName $ (Atomic "Pa")))
  (USynonym (UProd [(kilogram ^. usymb), (UPow (metre ^. usymb) (-1)),
                      (UPow (second ^. usymb) (-2))]))

newton = DUC
  (UD (dcc "newton" (cn' "Newton") "force") (UName $ Atomic "N"))
  (USynonym (UProd [(kilogram ^. usymb), (UPow (second ^. usymb) (-2))]))

millimetre = DUC
  (UD (dcc "millimetre" (cn' "Millimetre") "length")
      (UName $ (Atomic "mm")))
  (UScale 0.0001 (metre ^. usymb))

kilopascal = DUC
  (UD (dcc "kilopascal" (cn' "Kilopascal") "pressure")
      (UName $ Concat [Atomic "k", Atomic "Pa"]))
  (UScale 1000 (pascal ^. usymb))

radians = DUC
    (UD (dcc "radians" (cn' "Radians") "angle") (UName $ Atomic "rad"))
    (USynonym (metre /: metre))

-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (dcc "Degrees" "angle") (UName (Special Circle)))
  --  Equiv to pi/180 rad.

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
