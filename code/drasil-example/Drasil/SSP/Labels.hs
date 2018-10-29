module Drasil.SSP.Labels where

import Language.Drasil


-- Assumptions
slipSurfaceL, geoSlopeMatL, soilLayerHomoL, soilLayerIsoL,
    intersliceNormL, baseNormShearForL, stressStrainCurveL, planeStrainL,
    effectiveNormL, surfaceBaseSliceL :: Label
slipSurfaceL                = mkLabelRAAssump' "Slip-Surface-Concave"
geoSlopeMatL                = mkLabelRAAssump' "Geo-Slope-Mat-Props-of-Soil-Inputs"
soilLayerHomoL              = mkLabelRAAssump' "Soil-Layer-Homogeneous"
soilLayerIsoL               = mkLabelRAAssump' "Soil-Layers-Isotropic"
intersliceNormL             = mkLabelRAAssump' "Interslice-Norm-Shear-Forces-Linear"
baseNormShearForL           = mkLabelRAAssump' "Base-Norm-Shear-Forces-Linear-on-FS"
stressStrainCurveL          = mkLabelRAAssump' "Stress-Strain-Curve-interslice-Linear"
planeStrainL                = mkLabelRAAssump' "Plane-Strain-Conditions"
effectiveNormL              = mkLabelRAAssump' "Effective-Norm-Stress-Large"
surfaceBaseSliceL           = mkLabelRAAssump' "Surface-Base-Slice-between-Interslice-Straight-Lines"

-- Data Definition
sliceWghtL, baseWtrFL, surfWtrFL, intersliceWtrFL, angleAL, angleBL, lengthBL,
    lengthLbL, lengthLsL, seismicLoadFL, surfLoadsL, intrsliceFL, resShearWOL,
    mobShearWOL :: Label
sliceWghtL        = mkLabelSame "sliceWght" (Def DD)
baseWtrFL         = mkLabelSame "baseWtrF" (Def DD)
surfWtrFL         = mkLabelSame "surfWtrF" (Def DD)
intersliceWtrFL   = mkLabelSame "intersliceWtrF" (Def DD)
angleAL           = mkLabelSame "angleA" (Def DD)
angleBL           = mkLabelSame "angleB" (Def DD)
lengthBL          = mkLabelSame "lengthB" (Def DD)
lengthLbL         = mkLabelSame "lengthLb" (Def DD)
lengthLsL         = mkLabelSame "lengthLs" (Def DD)
seismicLoadFL     = mkLabelSame "seismicLoadF" (Def DD)
surfLoadsL        = mkLabelSame "surfLoads" (Def DD)
intrsliceFL       = mkLabelSame "intrsliceF" (Def DD)
resShearWOL       = mkLabelSame "resShearWO" (Def DD)
mobShearWOL       = mkLabelSame "mobShearWO" (Def DD)


-- General Definations
genDef1Label, genDef2Label, genDef3Label, genDef4Label, genDef5Label, genDef6Label 
    :: Label

genDef1Label  = mkLabelSame "normForcEq"  (Def General)
genDef2Label  = mkLabelSame "bsShrFEq"    (Def General)
genDef3Label  = mkLabelSame "resShr"      (Def General)
genDef4Label  = mkLabelSame "mobShr"      (Def General)
genDef5Label  = mkLabelSame "normShrR"    (Def General)
genDef6Label  = mkLabelSame "momentEql"   (Def General)

-- Instance Models
fctSftyL, nrmShrForL, inslideFxL, crtSlpIdL :: Label
fctSftyL = mkLabelSame "fctSfty"    (Def Instance)
nrmShrForL = mkLabelSame "nrmShrFor"  (Def Instance)
inslideFxL = mkLabelSame "inslideFx"  (Def Instance)
crtSlpIdL = mkLabelSame "crtSlpId"   (Def Instance)
