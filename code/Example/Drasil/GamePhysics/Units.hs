module Drasil.GamePhysics.Units where

import Language.Drasil
import Data.Drasil.SI_Units

import Control.Lens ((^.))

----- Table of Units -----

cpSIUnits :: [UnitDefn]
cpSIUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]

----- Derived Units -----

--m^3/kgs^2--
gravConstU :: DerUChunk
gravConstU = makeDerU (unitCon "gravitational constant") gravConst_eqn

gravConst_eqn :: UDefn
gravConst_eqn = USynonym (UDiv (m_3 ^. unit)
                               (UProd [kilogram ^. unit, s_2 ^. unit]))

