module Drasil.SWHS.Labels where

import Language.Drasil

assump1Label, assump10Label, assump14Label, assump18Label, 
  assump19Label :: Label

assump1Label = mkLabelRA'' "Thermal-Energy-Only"

assump10Label = mkLabelRA'' "Law-Convective-Cooling-Water-PCM"

assump14Label = mkLabelRA'' "Water-Always-Liquid"

assump18Label = mkLabelRA'' "No-Gaseous-State-PCM"

assump19Label = mkLabelRA'' "Atmospheric-Pressure-Tank"

traceFig1LC, traceFig2LC :: Label
traceFig1LC = mkLabelRA'' "traceFig1LabelSWHS"
traceFig2LC = mkLabelRA'' "traceFig2LabelSWHS"