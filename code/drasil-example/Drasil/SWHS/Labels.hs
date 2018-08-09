module Drasil.SWHS.Labels where

import Language.Drasil

assump1Label, assump10Label, assump14Label, assump18Label, 
  assump19Label :: Label

assump1Label  = mkLabelRAAssump' "Thermal-Energy-Only"
assump10Label = mkLabelRAAssump' "Law-Convective-Cooling-Water-PCM"
assump14Label = mkLabelRAAssump' "Water-Always-Liquid"
assump18Label = mkLabelRAAssump' "No-Gaseous-State-PCM"
assump19Label = mkLabelRAAssump' "Atmospheric-Pressure-Tank"

genDef1Label, genDef2Label :: Label
genDef1Label = mkLabelSame "nwtnCooling" (Def General)
genDef2Label = mkLabelSame "rocTempSimp" (Def General)

imod1Label, imod2Label, imod3Label, imod4Label :: Label
imod1Label = mkLabelSame "eBalanceOnWtr" (Def Instance)
imod2Label = mkLabelSame "eBalanceOnPCM" (Def Instance)
imod3Label = mkLabelSame "heatEInWtr"    (Def Instance)
imod4Label = mkLabelSame "heatEInPCM"    (Def Instance)

traceFig1LC, traceFig2LC :: Label
traceFig1LC = mkLabelRAFig "traceFig1LabelSWHS"
traceFig2LC = mkLabelRAFig "traceFig2LabelSWHS"