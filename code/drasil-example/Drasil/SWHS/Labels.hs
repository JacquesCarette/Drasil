module Drasil.SWHS.Labels where

import Language.Drasil

-- Assumptions
thermalEnergyOnlyL, lawConvectiveCoolingWtrPCML, waterAlwaysLiquidL, 
    noGaseousStatePCML, atmosphericPressureTankL :: Reference
heatTransferCoeffL, contantWaterTempL, tempPcmConsL, densityWaterL, specificHeatL,
  newtoLawConvecL, tempOverTimeL, tempOverLengthL, chargeTankL, sameInitialL,
  pcmInitialSolidL, perfectInsulationL, noInternalHeatL, volumeChangeMeltL,
  volumeCoilL :: Reference
thermalEnergyOnlyL          = makeAssumpRef "Thermal-Energy-Only"
heatTransferCoeffL          = makeAssumpRef "Heat-Transfer-Coeffs-Constant"
contantWaterTempL           = makeAssumpRef "Constant-Water-Temp-Across-Tank"
tempPcmConsL                = makeAssumpRef "Temp-PCM-Constant-Across-Volume"
densityWaterL               = makeAssumpRef "Density-Water-PCM-Constant-over-Volume"
specificHeatL               = makeAssumpRef "Specific-Heat-Energy-Constant-over-Volume"
newtoLawConvecL             = makeAssumpRef "Newton-Law-Convective-Cooling-Coil-Water"
tempOverTimeL               = makeAssumpRef "Temp-Heating-Coil-Constant-over-Time"
tempOverLengthL             = makeAssumpRef "Temp-Heating-Coil-Constant-over-Length"
lawConvectiveCoolingWtrPCML = makeAssumpRef "Law-Convective-Cooling-Water-PCM"
chargeTankL                 = makeAssumpRef "Charging-Tank-No-Temp-Discharge"
sameInitialL                = makeAssumpRef "Same-Initial-Temp-Water-PCM"
pcmInitialSolidL            = makeAssumpRef "PCM-Initially-Solid"
waterAlwaysLiquidL          = makeAssumpRef "Water-Always-Liquid"
perfectInsulationL          = makeAssumpRef "Perfect-Insulation-Tank"
noInternalHeatL             = makeAssumpRef "No-Internal-Heat-Generation-By-Water-PCM"
volumeChangeMeltL           = makeAssumpRef "Volume-Change-Melting-PCM-Negligible"
noGaseousStatePCML          = makeAssumpRef "No-Gaseous-State-PCM"
atmosphericPressureTankL    = makeAssumpRef "Atmospheric-Pressure-Tank"
volumeCoilL                 = makeAssumpRef "Volume-Coil-Negligible"

-- Data Definition
dd1HtFluxCL, dd2HtFluxPL, dd3HtFusionL, dd4MeltFracL :: Label
dd1HtFluxCL = mkLabelSame "ht_flux_C" (Def DD)
dd2HtFluxPL = mkLabelSame "ht_flux_P" (Def DD)
dd3HtFusionL = mkLabelSame "htFusion" (Def DD)
dd4MeltFracL = mkLabelSame "melt_frac" (Def DD)

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

-- Table
inputInitQuantsLbl :: Label
inputInitQuantsLbl = mkLabelSame "Input-Variable-Requirements" Tab
