module Data.Drasil.Units.Thermodynamics where

import Language.Drasil (dccWDS, cnIES, cn, cn', cn'', dcc, Sentence(S),
  UnitDefn, (/:), (*:), (/$), new_unit, makeDerU)

import Data.Drasil.SI_Units (centigrade, joule, kilogram, watt, m_2, m_3)

heat_capacity :: UnitDefn
heat_capacity = makeDerU (dccWDS "heat_capacity" (cnIES "heat capacity")
  (S "heat capacity (constant pressure)")) (joule /: centigrade)

heatCapSpec :: UnitDefn --Specific heat capacity
heatCapSpec = makeDerU (dccWDS "heatCapSpec" (cn' "specific heat")
  (S "heat capacity per unit mass")) (joule /$ (kilogram *: centigrade))

thermal_flux :: UnitDefn
thermal_flux = makeDerU (dccWDS "thermal_flux" (cn'' "heat flux")
  (S "the rate of heat energy transfer per unit area")) (watt /: m_2)

heat_transfer_coef :: UnitDefn
heat_transfer_coef = new_unit "heat transfer coefficient" $ (watt /$ (m_2 *: centigrade))

volHtGenU :: UnitDefn
volHtGenU = makeDerU (dcc "volHtGenU" (cn "volumetric heat generation")
  "the rate of heat energy generation per unit volume") (watt /: m_3)
