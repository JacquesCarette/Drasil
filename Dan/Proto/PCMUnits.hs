{-# OPTIONS -Wall #-}
module PCMUnits where

import SI_Units
import Unit (Unit(..), USymb(..), UDefn(..), DerUChunk(..),
  makeDerU, unitCon)

import Control.Lens ((^.))

--m^2--
m_2 :: DerUChunk
m_2 = makeDerU (unitCon "square metres") m_2eqn

m_2eqn :: UDefn
m_2eqn = USynonym (UPow (metre ^. unit) (2))

--J/(kg*C)--
heat_capacity :: DerUChunk
heat_capacity = makeDerU (unitCon "heat capacity") heat_cap_eqn

heat_cap_eqn :: UDefn
heat_cap_eqn = USynonym (UDiv 
  (joule ^. unit) (UProd [kilogram ^. unit, centigrade ^. unit]))

--W/m^2--
thermFluxU :: DerUChunk
thermFluxU = makeDerU (unitCon "flux") thermFluxUeqn

thermFluxUeqn :: UDefn
thermFluxUeqn = USynonym (UDiv (watt ^. unit) (m_2 ^. unit))

--W/(m^2C)--  
heat_transfer :: DerUChunk
heat_transfer = makeDerU (unitCon "heat transfer") heat_transfer_eqn

heat_transfer_eqn :: UDefn
heat_transfer_eqn = USynonym (UProd 
  [kilogram ^. unit, UPow (second ^. unit) (-3),
   UPow (centigrade ^. unit) (-1)])