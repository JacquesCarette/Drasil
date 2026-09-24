-- | Units related to the field of thermodynamics.
module Data.Drasil.Units.Thermodynamics where

import Drasil.Database (mkUid)
import Language.Drasil (cnIES, cn, cn', cn'', Sentence(S),
  UnitDefn, (/:), (*:), (/$), compoundUnit', compoundUnit, cncpt''')

import Data.Drasil.SI_Units (centigrade, joule, kilogram, watt, m_2, m_3)

heatCapacity :: UnitDefn
heatCapacity = compoundUnit' (cncpt''' (mkUid "heatCapacity") (cnIES "heat capacity")
  (S "heat capacity (constant pressure)")) (joule /: centigrade)

heatCapSpec :: UnitDefn --Specific heat capacity
heatCapSpec = compoundUnit' (cncpt''' (mkUid "heatCapSpec") (cn' "specific heat")
  (S "heat capacity per unit mass")) (joule /$ (kilogram *: centigrade))

thermalFlux :: UnitDefn
thermalFlux = compoundUnit' (cncpt''' (mkUid "thermalFlux") (cn'' "heat flux")
  (S "the rate of heat energy transfer per unit area")) (watt /: m_2)

heatTransferCoef :: UnitDefn
heatTransferCoef = compoundUnit' "heat transfer coefficient" (watt /$ (m_2 *: centigrade))

volHtGenU :: UnitDefn
volHtGenU = compoundUnit' (cncpt''' (mkUid "volHtGenU") (cn "volumetric heat generation")
  (S "the rate of heat energy generation per unit volume")) (watt /: m_3)
