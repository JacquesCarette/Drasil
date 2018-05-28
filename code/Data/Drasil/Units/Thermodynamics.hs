module Data.Drasil.Units.Thermodynamics where

import Language.Drasil
import Data.Drasil.SI_Units

heat_capacity :: UnitDefn
heat_capacity = makeDerU (dccWDS "heat_capacity" (cnIES "heat capacity")
  (S "heat capacity (constant pressure)")) $ 
  USynonym (joule /: centigrade)

heat_cap_spec :: UnitDefn --Specific heat capacity
heat_cap_spec = makeDerU (dccWDS "heat_cap_spec" (cn' "specific heat")
  (S "heat capacity per unit mass")) $
  USynonym (joule /$ (kilogram *: centigrade))

thermal_flux :: UnitDefn
thermal_flux = makeDerU (dccWDS "thermal_flux" (cn'' "heat flux")
  (S "the rate of heat energy transfer per unit area")) $ USynonym (watt /: m_2)

heat_transfer_coef :: UnitDefn
heat_transfer_coef = new_unit "heat transfer coefficient" $ (watt /$ (m_2 *: centigrade))

volHtGenU :: UnitDefn
volHtGenU = makeDerU (dcc "volHtGenU" (cn "volumetric heat generation")
  "the rate of heat energy generation per unit volume") $ USynonym (watt /: m_3)
