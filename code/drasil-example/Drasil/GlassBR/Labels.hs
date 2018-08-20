module Drasil.GlassBR.Labels where

import Language.Drasil

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Label

glassTypeL      = mkLabelRAAssump' "glassType"
glassConditionL = mkLabelRAAssump' "glassCondition"
glassLiteL      = mkLabelRAAssump' "glassLite"

-- Instance Models
probOfBreakL, calOfCapacityL, calOfDemandL :: Label

probOfBreakL   = mkLabelSame "probOfBreak"   (Def Instance)
calOfCapacityL = mkLabelSame "calofCapacity" (Def Instance)
calOfDemandL   = mkLabelSame "calOfDemand"   (Def Instance)

-- Sections MIS
hwLabel, constantsLabel, outputLabel, seqServLabel, contoursLabel,
  functLabel, thicknessLabel, glTypeLabel, calcLabel, loadLabel,
  inputLabel, ctrlLabel :: Label

hwLabel        = mkLabelRASec "HW_MIS" "Hardware"
constantsLabel = mkLabelRASec "Constants_MIS" "Constants"
outputLabel    = mkLabelRASec "Output_MIS" "Output"
seqServLabel   = mkLabelRASec "SeqServices_MIS" "SeqServices"
contoursLabel  = mkLabelRASec "ContoursADT_MIS" "ContoursADT"
functLabel     = mkLabelRASec "FunctADT_MIS" "FunctADT"
thicknessLabel = mkLabelRASec "ThicknessADT_MIS" "ThicknessADT"
glTypeLabel    = mkLabelRASec "GlassTypeADT_MIS" "GlassTypeADT"
calcLabel      = mkLabelRASec "Calc_MIS" "Calc"
loadLabel      = mkLabelRASec "LoadASTM_MIS" "LoadASTM"
inputLabel     = mkLabelRASec "Input_MIS" "Input"
ctrlLabel      = mkLabelRASec "Control_MIS" "Control"
