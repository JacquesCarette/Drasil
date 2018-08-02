module Drasil.SWHS.Labels where

import Language.Drasil

assump1Label, assump10Label, assump14Label, assump18Label, 
  assump19Label :: Label

assump1Label = mkLabelRAAssump (mkLabelRA'' "Thermal-Energy-Only")

assump10Label = mkLabelRAAssump (mkLabelRA'' "Law-Convective-Cooling-Water-PCM")

assump14Label = mkLabelRAAssump (mkLabelRA'' "Water-Always-Liquid")

assump18Label = mkLabelRAAssump (mkLabelRA'' "No-Gaseous-State-PCM")

assump19Label = mkLabelRAAssump (mkLabelRA'' "Atmospheric-Pressure-Tank")

traceFig1LC, traceFig2LC :: Label
traceFig1LC = mkLabelRAFig "traceFig1LabelSWHS"
traceFig2LC = mkLabelRAFig "traceFig2LabelSWHS"