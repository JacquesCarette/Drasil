{-# OPTIONS -Wall #-}
module Drasil.SWHS.Units where

import Language.Drasil
import Data.Drasil.SI_Units

import Control.Lens ((^.))

--J/kg--
specificE :: DerUChunk
specificE = makeDerU (dcc "specificE" "specific energy" "energy per unit mass") 
            specificE_eqn

specificE_eqn ::UDefn
specificE_eqn = USynonym (UDiv (joule ^. unit) (kilogram ^. unit))

--W/m^3--
volHtGenU :: DerUChunk
volHtGenU = makeDerU (dcc "volHtGenU" "volumetric heat generation" 
  "the rate of heat energy generation per unit volume") volHtGenUeqn
  
volHtGenUeqn :: UDefn
volHtGenUeqn = USynonym (UDiv (watt ^. unit) (m_3 ^. unit))
