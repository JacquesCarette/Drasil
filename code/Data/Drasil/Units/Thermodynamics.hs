module Data.Drasil.Units.Thermodynamics where

import Language.Drasil
import Data.Drasil.SI_Units
import Control.Lens ((^.))

heat_capacity :: DerUChunk
heat_capacity = makeDerU (dccWDS "heat_capacity" (cnIES "heat capacity")
  (S "heat capacity (constant pressure)")) $ 
  USynonym (joule /: centigrade)

heat_cap_spec :: DerUChunk --Specific heat capacity
heat_cap_spec = makeDerU (dccWDS "heat_cap_spec" (cn' "specific heat")
  (S "heat capacity per unit mass")) $
  USynonym (UProd [joule ^. usymb, kilogram ^: (-1), centigrade ^: (-1)])

thermal_flux :: DerUChunk
thermal_flux = makeDerU (dccWDS "thermal_flux" (cn'' "heat flux")
  (S "the rate of heat energy transfer per unit area")) $ USynonym (watt /: m_2)

heat_transfer_coef :: DerUChunk
heat_transfer_coef = new_unit "heat transfer coefficient" $ 
  (UProd [watt ^. usymb, m_2 ^: (-1), centigrade ^: (-1)])

volHtGenU :: DerUChunk
volHtGenU = makeDerU (dcc "volHtGenU" (cn "volumetric heat generation")
  "the rate of heat energy generation per unit volume") $ USynonym (watt /: m_3)
