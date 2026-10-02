-- | Units related to the field of thermodynamics.
module Data.Drasil.Units.Thermodynamics where

import Language.Drasil (cnIES, cn, cn', cn'',
  UnitDefn, (/:), (*:), (/$), compoundUnit)

import Data.Drasil.SI_Units (centigrade, joule, kilogram, watt, m_2, m_3)

heatCapacity :: UnitDefn
heatCapacity = compoundUnit "heatCapacity" (cnIES "heat capacity")
  "heat capacity (constant pressure)" (joule /: centigrade)

heatCapSpec :: UnitDefn --Specific heat capacity
heatCapSpec = compoundUnit "heatCapSpec" (cn' "specific heat")
  "heat capacity per unit mass" (joule /$ (kilogram *: centigrade))

thermalFlux :: UnitDefn
thermalFlux = compoundUnit "thermalFlux" (cn'' "heat flux")
  "the rate of heat energy transfer per unit area" (watt /: m_2)

heatTransferCoef :: UnitDefn
heatTransferCoef = compoundUnit "heat transfer coefficient" (cn' "heat transfer coefficient")
  "heat transfer coefficient" (watt /$ (m_2 *: centigrade))

volHtGenU :: UnitDefn
volHtGenU = compoundUnit "volHtGenU" (cn "volumetric heat generation")
  "the rate of heat energy generation per unit volume" (watt /: m_3)
