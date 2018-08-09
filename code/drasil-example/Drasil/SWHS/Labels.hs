module Drasil.SWHS.Labels where

import Language.Drasil

assump1Label, assump10Label, assump14Label, assump18Label, 
  assump19Label :: Label

assump1Label  = mkLabelRAAssump' "Thermal-Energy-Only"

assump10Label = mkLabelRAAssump' "Law-Convective-Cooling-Water-PCM"

assump14Label = mkLabelRAAssump' "Water-Always-Liquid"

assump18Label = mkLabelRAAssump' "No-Gaseous-State-PCM"

assump19Label = mkLabelRAAssump' "Atmospheric-Pressure-Tank"

traceFig1LC, traceFig2LC :: Label
traceFig1LC = mkLabelRAFig "traceFig1LabelSWHS"
traceFig2LC = mkLabelRAFig "traceFig2LabelSWHS"