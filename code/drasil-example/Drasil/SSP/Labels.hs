module Drasil.SSP.Labels where

import Language.Drasil

genDef1Label, genDef2Label, genDef3Label, genDef4Label, genDef5Label, genDef6Label, 
    genDef7Label, genDef8Label, genDef9Label, genDef10Label :: Label

genDef1Label  = mkLabelSame "normForcEq"  (Def General)
genDef2Label  = mkLabelSame "bsShrFEq"    (Def General)
genDef3Label  = mkLabelSame "resShr"      (Def General)
genDef4Label  = mkLabelSame "mobShr"      (Def General)
genDef5Label  = mkLabelSame "normShrR"    (Def General)
genDef6Label  = mkLabelSame "momentEql"   (Def General)
genDef7Label  = mkLabelSame "netForcex"   (Def General)
genDef8Label  = mkLabelSame "netForcey"   (Def General)
genDef9Label  = mkLabelSame "hookesLaw2d" (Def General)
genDef10Label = mkLabelSame "displVect"   (Def General)