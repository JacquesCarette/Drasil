module Data.Drasil.SI_Units where
import Language.Drasil.Chunk (makeCC)
import Language.Drasil.Unit (Unit(..), UDefn(..), FundUnit(..), DerUChunk(..),
  UnitDefn(..), new_unit, (^:), (/:), (*:), makeDerU)
import Language.Drasil.Unicode (Special(Circle))
import Language.Drasil.Symbol
import Language.Drasil.Spec (USymb(..))

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
fund nam desc sym = UD (makeCC nam desc) (UName $ Atomic sym)

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
  (UD (makeCC "Centigrade" "temperature") 
      (UName (Concat [Special Circle, Atomic "C"])))
  (UShift 273.15 (kelvin ^. unit))

joule = DUC
    (UD (makeCC "Joule" "energy") (UName $ Atomic "J"))
    (USynonym (UProd [kilogram ^. unit, m_2 ^. unit,
                      UPow (second ^. unit) (-2)]))

calorie = DUC
  (UD (makeCC "Calorie" "energy") (UName $ Atomic "cal"))
  (UScale 4.184 (joule ^. unit))

watt = DUC
  (UD (makeCC "Watt" "power") (UName $ Atomic "W"))
  (USynonym (UProd [kilogram ^. unit, m_2 ^. unit,
                    UPow (second ^. unit) (-3)]))

kilowatt = DUC
  (UD (makeCC "Kilowatt" "power")
      (UName $ Concat [Atomic "k", Atomic "W"]))
  (UScale 1000 (watt ^. unit))

pascal = DUC
  (UD (makeCC "Pascal" "pressure") (UName $ (Atomic "Pa")))
  (USynonym (UProd [(kilogram ^. unit), (UPow (metre ^. unit) (-1)),
                      (UPow (second ^. unit) (-2))]))

newton = DUC
  (UD (makeCC "Newton" "force") (UName $ Atomic "N"))
  (USynonym (UProd [(kilogram ^. unit), (UPow (second ^. unit) (-2))]))

millimetre = DUC
  (UD (makeCC "Millimetre" "length")
      (UName $ (Atomic "mm")))
  (UScale 0.0001 (metre ^. unit))

kilopascal = DUC
  (UD (makeCC "Kilopascal" "pressure")
      (UName $ Concat [Atomic "k", Atomic "Pa"]))
  (UScale 1000 (pascal ^. unit))

radians = DUC
    (UD (makeCC "Radians" "angle") (UName $ Atomic "rad"))
    (USynonym (metre /: metre))

-- FIXME: Need to add pi 
--degrees = DUC
  --  (UD (makeCC "Degrees" "angle") (UName (Special Circle)))
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
gravConstU = makeDerU (makeCC "gravitational constant" "universal gravitational constant") $
   USynonym (UDiv (m_3 ^. unit) (UProd [kilogram ^. unit, s_2 ^. unit]))

