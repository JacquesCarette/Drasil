module Drasil.SWHS.Labels where

import Language.Drasil

-- Assumptions
thermalEnergyOnlyL, lawConvectiveCoolingWtrPCML, waterAlwaysLiquidL, 
    noGaseousStatePCML, atmosphericPressureTankL :: Label
thermalEnergyOnlyL          = mkLabelRAAssump' "Thermal-Energy-Only"
lawConvectiveCoolingWtrPCML = mkLabelRAAssump' "Law-Convective-Cooling-Water-PCM"
waterAlwaysLiquidL          = mkLabelRAAssump' "Water-Always-Liquid"
noGaseousStatePCML          = mkLabelRAAssump' "No-Gaseous-State-PCM"
atmosphericPressureTankL    = mkLabelRAAssump' "Atmospheric-Pressure-Tank"

-- General Definitions
nwtnCoolingL, rocTempSimpL :: Label
nwtnCoolingL = mkLabelSame "nwtnCooling" (Def General)
rocTempSimpL = mkLabelSame "rocTempSimp" (Def General)

-- Instance Models
eBalanceOnWtrL, eBalanceOnPCML, heatEInWtrL, heatEInPCML :: Label
eBalanceOnWtrL = mkLabelSame "eBalanceOnWtr" (Def Instance)
eBalanceOnPCML = mkLabelSame "eBalanceOnPCM" (Def Instance)
heatEInWtrL    = mkLabelSame "heatEInWtr"    (Def Instance)
heatEInPCML    = mkLabelSame "heatEInPCM"    (Def Instance)