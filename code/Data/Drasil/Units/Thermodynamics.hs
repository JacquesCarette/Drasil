module Data.Drasil.Units.Thermodynamics where

import Language.Drasil
import Data.Drasil.SI_Units
import Control.Lens ((^.))

heat_capacity :: DerUChunk
heat_capacity = makeDerU (dccWDS "heat_capacity" "heat capacity" 
  (S "heat capacity (constant pressure)")) $ 
  USynonym (joule /: centigrade)

heat_cap_spec :: DerUChunk --Specific heat capacity
heat_cap_spec = makeDerU (dccWDS "heat_cap_spec" "specific heat" 
  (S "heat capacity per unit mass")) $
  USynonym (UDiv (joule ^. unit) (UProd [kilogram ^. unit, centigrade ^. unit]))

thermal_flux :: DerUChunk
thermal_flux = makeDerU (dccWDS "thermal_flux" "heat flux"
  (S "the rate of heat energy transfer per unit area")) $ USynonym (watt /: m_2)

heat_transfer_coef :: DerUChunk
heat_transfer_coef = new_unit "heat transfer coefficient" $ (UDiv
  (watt ^. unit) (UProd [m_2 ^. unit, centigrade ^. unit]))

volHtGenU :: DerUChunk
volHtGenU = makeDerU (dcc "volHtGenU" "volumetric heat generation" 
  "the rate of heat energy generation per unit volume") $ USynonym (watt /: m_3)
