module Drasil.SSP.Labels where

import Language.Drasil


-- Assumptions
slipSurfaceL, geoSlopeMatL, soilLayerHomoL, soilLayerIsoL,
    intersliceNormL, baseNormShearForL, stressStrainCurveL, planeStrainL,
    effectiveNormL, surfaceBaseSliceL :: Reference
slipSurfaceL                = makeAssumpRef "Slip-Surface-Concave"
geoSlopeMatL                = makeAssumpRef "Geo-Slope-Mat-Props-of-Soil-Inputs"
soilLayerHomoL              = makeAssumpRef "Soil-Layer-Homogeneous"
soilLayerIsoL               = makeAssumpRef "Soil-Layers-Isotropic"
intersliceNormL             = makeAssumpRef "Interslice-Norm-Shear-Forces-Linear"
baseNormShearForL           = makeAssumpRef "Base-Norm-Shear-Forces-Linear-on-FS"
stressStrainCurveL          = makeAssumpRef "Stress-Strain-Curve-interslice-Linear"
planeStrainL                = makeAssumpRef "Plane-Strain-Conditions"
effectiveNormL              = makeAssumpRef "Effective-Norm-Stress-Large"
surfaceBaseSliceL           = makeAssumpRef "Surface-Base-Slice-between-Interslice-Straight-Lines"

-- Data Definition
sliceWghtL, baseWtrFL, surfWtrFL, intersliceWtrFL, angleAL, angleBL, lengthBL,
    lengthLbL, lengthLsL, seismicLoadFL, surfLoadsL, intrsliceFL, resShearWOL,
    mobShearWOL :: Reference
sliceWghtL        = makeDDRef "sliceWght"
baseWtrFL         = makeDDRef "baseWtrF"
surfWtrFL         = makeDDRef "surfWtrF"
intersliceWtrFL   = makeDDRef "intersliceWtrF"
angleAL           = makeDDRef "angleA"
angleBL           = makeDDRef "angleB"
lengthBL          = makeDDRef "lengthB"
lengthLbL         = makeDDRef "lengthLb"
lengthLsL         = makeDDRef "lengthLs"
seismicLoadFL     = makeDDRef "seismicLoadF"
surfLoadsL        = makeDDRef "surfLoads"
intrsliceFL       = makeDDRef "intrsliceF"
resShearWOL       = makeDDRef "resShearWO"
mobShearWOL       = makeDDRef "mobShearWO"


-- General Definations
genDef2Label, genDef3Label, genDef4Label, genDef5Label, genDef6Label 
    :: Reference

genDef2Label  = makeGDRef "bsShrFEq"
genDef3Label  = makeGDRef "resShr"
genDef4Label  = makeGDRef "mobShr"
genDef5Label  = makeGDRef "normShrR"
genDef6Label  = makeGDRef "momentEql"

-- Instance Models
fctSftyL, nrmShrForL, inslideFxL, crtSlpIdL :: Reference
fctSftyL = makeInstRef "fctSfty"
nrmShrForL = makeInstRef "nrmShrFor"
inslideFxL = makeInstRef "inslideFx"
crtSlpIdL = makeInstRef "crtSlpId"

-- Fig
forceDiagramL :: Reference
forceDiagramL = makeFigRef "ForceDiagram"
