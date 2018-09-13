module Drasil.SWHS.Labels where

import Language.Drasil

-- Assumptions
thermalEnergyOnlyL, lawConvectiveCoolingWtrPCML, waterAlwaysLiquidL, 
    noGaseousStatePCML, atmosphericPressureTankL :: Label
thermalEnergyOnlyL          = mkLabelRAAssump' "Thermal-Energy-Only"
heatTransferCoeffL          = mkLabelRAAssump' "Heat-Transfer-Coeffs-Constant"
contantWaterTempL           = mkLabelRAAssump' "Constant-Water-Temp-Across-Tank"
tempPcmConsL                = mkLabelRAAssump' "Temp-PCM-Constant-Across-Volume"
densityWaterL               = mkLabelRAAssump' "Density-Water-PCM-Constant-over-Volume"
specificHeatL               = mkLabelRAAssump' "Specific-Heat-Energy-Constant-over-Volume"
newtoLawConvecL             = mkLabelRAAssump' "Newton-Law-Convective-Cooling-Coil-Water"
tempOverTimeL               = mkLabelRAAssump' "Temp-Heating-Coil-Constant-over-Time"
tempOverLengthL             = mkLabelRAAssump' "Temp-Heating-Coil-Constant-over-Length"
lawConvectiveCoolingWtrPCML = mkLabelRAAssump' "Law-Convective-Cooling-Water-PCM"
chargeTankL                 = mkLabelRAAssump' "Charging-Tank-No-Temp-Discharge"
sameInitialL                = mkLabelRAAssump' "Same-Initial-Temp-Water-PCM"
pcmInitialSolidL            = mkLabelRAAssump' "PCM-Initially-Solid"
waterAlwaysLiquidL          = mkLabelRAAssump' "Water-Always-Liquid"
perfectInsulationL          = mkLabelRAAssump' "Perfect-Insulation-Tank"
noInternalHeatL             = mkLabelRAAssump' "No-Internal-Heat-Generation-By-Water-PCM"
volumeChangeMeltL           = mkLabelRAAssump' "Volume-Change-Melting-PCM-Negligible"
noGaseousStatePCML          = mkLabelRAAssump' "No-Gaseous-State-PCM"
atmosphericPressureTankL    = mkLabelRAAssump' "Atmospheric-Pressure-Tank"
volumeCoilL                  = mkLabelRAAssump' "Volume-Coil-Negligible"

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

-- Likely Changes and Unlikely Changes
likeChg1L, likeChg2L, likeChg3L, likeChg4L, likeChg5L,likeChg6L
  , unlikeChg1L, unlikeChg2L :: Label
likeChg1L = mkLabelSame "Uniform-Temperature-PCM"  LCh
likeChg2L = mkLabelSame "Temperature-Coil-Variable-Over-Day"  LCh
likeChg3L = mkLabelSame "Temperature-Coil-Variable-Over-Length"  LCh
likeChg4L = mkLabelSame "Discharging-Tank"  LCh
likeChg5L = mkLabelSame "Different-Initial-Temps-PCM-Water"  LCh
likeChg6L = mkLabelSame "Tank-Lose-Heat"  LCh

unlikeChg1L = mkLabelSame "Water-PCM-Fixed-States" UnCh
unlikeChg2L = mkLabelSame "No-Internal-Heat-Generation" UnCh

-- Requirements
inputInitQuantsL, useAboveFindMassL, checkWithPhysConstsL, 
  outputInputDerivQuantsL, calcTempWtrOverTimeL, calcTempPCMOverTimeL,
  calcChgHeatEnergyWtrOverTimeL, calcChgHeatEnergyPCMOverTimeL,
  verifyEnergyOutputL, calcPCMMeltBeginL, calcPCMMeltEndL :: Label

inputInitQuantsL = mkLabelSame "Input-Initial-Quantities" (Req FR)
useAboveFindMassL = mkLabelSame "Use-Above-Find-Mass-IM1-IM4" (Req FR)
checkWithPhysConstsL = mkLabelSame "Check-Input-with-Physical_Constraints" (Req FR)
outputInputDerivQuantsL = mkLabelSame "Output-Input-Derived-Quantities" (Req FR)
calcTempWtrOverTimeL = mkLabelSame "Calculate-Temperature-Water-Over-Time" (Req FR)
calcTempPCMOverTimeL = mkLabelSame "Calculate-Temperature-PCM-Over-Time" (Req FR)
calcChgHeatEnergyWtrOverTimeL = mkLabelSame "Calculate-Change-Heat_Energy-Water-Over-Time" (Req FR)
calcChgHeatEnergyPCMOverTimeL = mkLabelSame "Calculate-Change-Heat_Energy-PCM-Over-Time" (Req FR)
verifyEnergyOutputL = mkLabelSame "Verify-Energy-Output-follow-Conservation-of-Energy" (Req FR)
calcPCMMeltBeginL = mkLabelSame "Calculate-PCM-melt-begin-time" (Req FR)
calcPCMMeltEndL = mkLabelSame "Calculate-PCM-melt-end-time" (Req FR)

