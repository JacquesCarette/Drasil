{-# OPTIONS -Wall #-}
module Example.Drasil.SWHS.Units where

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

--m^2--
m_2 :: DerUChunk
m_2 = makeDerU (unitCon "square metres") m_2eqn

m_2eqn :: UDefn
m_2eqn = USynonym (UPow (metre ^. unit) (2))

--m^2--
m_3 :: DerUChunk
m_3 = makeDerU (unitCon "cubic metres") m_3eqn

m_3eqn :: UDefn
m_3eqn = USynonym (UPow (metre ^. unit) (3))

--kg/m^3--
densityU :: DerUChunk
densityU = makeDerU (unitCon "density") densityU_eqn

densityU_eqn :: UDefn
densityU_eqn = USynonym (UDiv (kilogram ^. unit) (m_3 ^. unit))

--J/kg--
specificE :: DerUChunk
specificE = makeDerU (CC "specific energy" (S "energy per unit mass")) 
            specificE_eqn

specificE_eqn ::UDefn
specificE_eqn = USynonym (UDiv (joule ^. unit) (kilogram ^. unit))

--J/(kg*C)--
heat_capacity :: DerUChunk
heat_capacity = makeDerU (CC "specific heat"
  (S "heat capacity per unit mass")) heat_cap_eqn

heat_cap_eqn :: UDefn
heat_cap_eqn = USynonym (UDiv 
  (joule ^. unit) (UProd [kilogram ^. unit, centigrade ^. unit]))

--W/m^2--
thermFluxU :: DerUChunk
thermFluxU = makeDerU (CC "heat flux" 
  (S "the rate of heat energy transfer per unit area")) thermFluxUeqn

thermFluxUeqn :: UDefn
thermFluxUeqn = USynonym (UDiv (watt ^. unit) (m_2 ^. unit))

--W/(m^2C)--  
heat_transfer :: DerUChunk
heat_transfer = makeDerU (unitCon "heat transfer") heat_transfer_eqn

heat_transfer_eqn :: UDefn
heat_transfer_eqn = USynonym (UDiv 
  (watt ^. unit) (UProd [m_2 ^. unit, centigrade ^. unit]))
  
--unitless--
unitless :: FundUnit
unitless = fund "unitless" "no units" "unitless"