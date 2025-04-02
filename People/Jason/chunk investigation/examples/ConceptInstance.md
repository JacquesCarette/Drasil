## Specific Examples

The following 3 examples are representative of the population:

Defining example goals/objectives. e.g.,
`drasil-example/swhs/lib/Drasil/SWHS/Goals.hs`:
```haskell
goals :: [ConceptInstance]
goals = [waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS]

waterTempGS :: ConceptInstance
waterTempGS = cic "waterTempGS" (goalState tempW) "Predict-Water-Temperature"
  goalStmtDom
```

Defining example requirements. e.g.,
`drasil-example/projectile/lib/Drasil/Projectile/Requirements.hs`:
```haskell
verifyInVals, calcValues, outputValues :: ConceptInstance

verifyInVals = cic "verifyInVals" verifyParamsDesc "Verify-Input-Values" funcReqDom
calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom
```
(the `*Desc`s are simple sentences).

Defining (Un)Likely changes. e.g.,
`drasil-example/glassbr/lib/Drasil/GlassBR/Changes.hs`:
```haskell
unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [predictWithstandOfCertDeg, accAlteredGlass]

predictWithstandOfCertDeg, accAlteredGlass :: ConceptInstance

predictWithstandOfCertDeg = cic "predictWithstandOfCertDeg" predictWithstandOfCertDegDesc "Predict-Withstanding-of-Certain-Degree"  unlikeChgDom
accAlteredGlass           = cic "accAlteredGlass"           accAlteredGlassDesc           "Accommodate-Altered-Glass"               unlikeChgDom

predictWithstandOfCertDegDesc, accAlteredGlassDesc :: Sentence

predictWithstandOfCertDegDesc = foldlSent [atStartNP (goal `the_ofThe` system),
  S "is to predict whether the", phrase glaSlab, S "under consideration can",
  S "withstand an", phrase explosion, S "of a certain degree"]

accAlteredGlassDesc = foldlSent [refS assumpGC, S "requires that the", phrase glass +:+.
  S "is not altered in any way", S "Therefore, this cannot be used on altered",
  phrase glass]
```

## Constructor Use

<details>

<summary>`rg "cic" -ths`</summary>

```console
drasil-lang/lib/Language/Drasil.hs
98:  , dcc, dccWDS, cc, cc', ccs, cw, cic

drasil-example/sglpend/lib/Drasil/SglPend/Goals.hs
24: motionMass = cic "motionMass" 

drasil-example/sglpend/lib/Drasil/SglPend/Requirements.hs
17:verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values" funcReqDom
18:calcAngPos = cic "calcAngPos" calcAngPosDesc "Calculate-Angular-Position-Of-Mass" funcReqDom
19:outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

drasil-docLang/lib/Drasil/Sections/Requirements.hs
62:inReq  s = cic "inputValues"  s "Input-Values"  funcReqDom
63:--outReq s = cic "inputValues" s "Output-Values" funcReqDom
105:mkMaintainableNFR refAddress percent lbl = cic refAddress (foldlSent [
115:mkPortableNFR refAddress [os] lbl = cic refAddress (S $ "The code shall be portable to " ++ os) lbl nonFuncReqDom
116:mkPortableNFR refAddress osList lbl = cic refAddress (foldlSent [
123:mkCorrectNFR refAddress lbl = cic refAddress (foldlSent [
130:mkVerifiableNFR refAddress lbl = cic refAddress (foldlSent [
136:mkUnderstandableNFR refAddress lbl = cic refAddress (foldlSent [
142:mkReusableNFR refAddress lbl = cic refAddress (foldlSent [
147:mkSecurityNFR refAddress lbl = cic refAddress (foldlSent [

drasil-example/swhs/lib/Drasil/SWHS/Goals.hs
15:waterTempGS = cic "waterTempGS" (goalState tempW) "Predict-Water-Temperature"
19:pcmTempGS = cic "pcmTempGS" (goalState tempPCM) "Predict-PCM-Temperature" 
23:waterEnergyGS = cic "waterEnergyGS" (goalState watE) "Predict-Water-Energy"
27:pcmEnergyGS = cic "pcmEnergyGS" (goalState pcmE) "Predict-PCM-Energy" 

drasil-example/pdcontroller/lib/Drasil/PDController/SpSysDesc.hs
34:  = cic "processVariable"

drasil-example/swhs/lib/Drasil/SWHS/Changes.hs
32:likeChgUTP = cic "likeChgUTP" (
38:likeChgTCVOD = cic "likeChgTCVOD" (
43:likeChgTCVOL = cic "likeChgTCVOL" (
48:likeChgDT = cic "likeChgDT" (
53:likeChgDITPW = cic "likeChgDITPW" (
59:likeChgTLH = cic "likeChgTLH" (
70:unlikeChgWPFS = cic "unlikeChgWPFS" (
76:unlikeChgNIHG = cic "unlikeChgNIHG" (
81:unlikeChgNGS = cic "unlikeChgNGS" (

drasil-example/pdcontroller/lib/Drasil/PDController/Changes.hs
15:likeChgPP = cic "likeChgPP" likeChgPPDesc "DC Gain and Time Constant" likeChgDom

drasil-example/swhs/lib/Drasil/SWHS/Assumptions.hs
42:assumpTEO = cic "assumpTEO"                  assumpS1                   "Thermal-Energy-Only"                       assumpDom
43:assumpHTCC = cic "assumpHTCC"                assumpS2                   "Heat-Transfer-Coeffs-Constant"             assumpDom
44:assumpCWTAT = cic "assumpCWTAT"              assumpS3                   "Constant-Water-Temp-Across-Tank"           assumpDom
45:assumpTPCAV = cic "assumpTPCAV"              assumpS4                   "Temp-PCM-Constant-Across-Volume"           assumpDom
46:assumpDWPCoV = cic "assumpDWPCoV"            assumpS5                   "Density-Water-PCM-Constant-over-Volume"    assumpDom
47:assumpSHECoV = cic "assumpSHECov"            assumpS6                   "Specific-Heat-Energy-Constant-over-Volume" assumpDom
48:assumpLCCCW = cic "assumpLCCCW"              assumpS7                   "Newton-Law-Convective-Cooling-Coil-Water"  assumpDom
49:assumpTHCCoT = cic "assumpTHCCoT"            assumpS8                   "Temp-Heating-Coil-Constant-over-Time"      assumpDom
50:assumpTHCCoL = cic "assumpTHCCoL"            assumpS9                   "Temp-Heating-Coil-Constant-over-Length"    assumpDom
51:assumpLCCWP = cic "assumpLCCWP"              assumpS10                  "Law-Convective-Cooling-Water-PCM"          assumpDom
52:assumpCTNOD = cic "assumpCTNOD"              assumpS11                  "Charging-Tank-No-Temp-Discharge"           assumpDom
53:assumpSITWP = cic "assumpSITWP"              assumpS12                  "Same-Initial-Temp-Water-PCM"               assumpDom
54:assumpPIS = cic "assumpPIS"                  assumpS13                  "PCM-Initially-Solid"                       assumpDom
55:assumpWAL = cic "assumpWAL"                  (assumpS14 $ phrase water) "Water-Always-Liquid"                       assumpDom
56:assumpPIT = cic "assumpPIT"                  assumpS15                  "Perfect-Insulation-Tank"                   assumpDom
57:assumpNIHGBWP = cic "assumpNIHGBWP"          assumpS16                  "No-Internal-Heat-Generation-By-Water-PCM"  assumpDom
58:assumpVCMPN = cic "assumpVCMPN"              assumpS17                  "Volume-Change-Melting-PCM-Negligible"      assumpDom
59:assumpNGSP = cic "assumpNGSP"                assumpS18                  "No-Gaseous-State-PCM"                      assumpDom
60:assumpAPT = cic "assumpAPT"                  assumpS19                  "Atmospheric-Pressure-Tank"                 assumpDom
61:assumpVCN = cic "assumpVCN"                  assumpS20                  "Volume-Coil-Negligible"                    assumpDom

drasil-example/projectile/lib/Drasil/Projectile/Goals.hs
13:targetHit = cic "targetHit" 

drasil-example/swhs/lib/Drasil/SWHS/Requirements.hs
63:findMassConstruct fr m ims ddefs = cic "findMass" (foldlSent [
69:checkWithPhysConsts = cic "checkWithPhysConsts" (foldlSent [
77:oIDQConstruct x = cic "outputInputDerivVals" (foldlSentCol [
93:calcValues l = cic "calcValues" (S "Calculate the following" +: plural value +:+.
96:verifyEnergyOutput = cic "verifyEnergyOutput" (foldlSent [
104:calcPCMMeltBegin = cic "calcPCMMeltBegin" (foldlSent [
110:calcPCMMeltEnd = cic "calcPCMMeltEnd" (foldlSent [
116:outputValues l = cic "outputValues" (titleize output_ +:+. outputList l)

drasil-example/pdcontroller/lib/Drasil/PDController/Assumptions.hs
22:aPwrPlant = cic "pwrPlant" pwrPlantDesc "Power plant" assumpDom
24:aDecoupled = cic "decoupled" aDecoupledDesc "Decoupled equation" assumpDom
26:aSP = cic "setPoint" aSPDesc "Set-Point" assumpDom
29:  = cic "externalDisturb" aExtDisturbDesc "External disturbance" assumpDom
31:aInitialValue = cic "initialValue" aInitialValueDesc "Initial Value" assumpDom
33:aParallelEq = cic "parallelEq" aParallelEqDesc "Parallel Equation" assumpDom
36:  = cic "pwrPlantTxFnx" apwrPlantTxFnxDesc "Transfer Function" assumpDom
39:  = cic "unfilteredDerivative" aUnfilteredDerivativeDesc "Unfiltered Derivative"
42:aMass = cic "massSpring" aMassDesc "Spring Mass" assumpDom
45:  = cic "dampingCoeffSpring" aDampingCoeffDesc "Spring Damping Coefficient"
49:  = cic "stiffnessCoeffSpring" aStiffnessCoeffDesc "Spring Stiffness Coefficient"

drasil-example/pdcontroller/lib/Drasil/PDController/Requirements.hs
18:  = cic "verifyInputs" verifyInputsDesc "Verify-Input-Values" funcReqDom
20:  = cic "calculateValues" calculateValuesDesc "Calculate-Values" funcReqDom
21:outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

drasil-example/dblpend/lib/Drasil/DblPend/Goals.hs
24:motionMass = cic "motionMass" 

drasil-lang/lib/Drasil/Language/Concept.hs
6:  , ConceptInstance(ConInst), cic
141:cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
142:cic u d sn dom = ConInst (ccs (nc u $ pn sn) d [dom]) u $ shortname' (S sn)

drasil-example/swhsnopcm/lib/Drasil/SWHSNoPCM/Changes.hs
22:likeChgDT = cic "likeChgDT" (
34:unlikeChgWFS = cic "unlikeChgWFS" (
40:unlikeChgNIHG = cic "unlikeChgNIHG" (

drasil-example/projectile/lib/Drasil/Projectile/Requirements.hs
25:verifyInVals = cic "verifyInVals" verifyParamsDesc "Verify-Input-Values" funcReqDom
26:calcValues   = cic "calcValues"   calcValuesDesc   "Calculate-Values"    funcReqDom
27:outputValues = cic "outputValues" outputValuesDesc "Output-Values"       funcReqDom

drasil-example/glassbr/lib/Drasil/GlassBR/Goals.hs
16:willBreakGS = cic "willBreakGS" (foldlSent [S "Analyze" `S.and_`

drasil-example/gamephysics/lib/Drasil/GamePhysics/Goals.hs
14:linearGS = cic "linearGS" (goalStatementStruct (take 2 outputSymbols)
18:angularGS = cic "angularGS" (goalStatementStruct (drop 3 $ take 5 inputSymbols)

drasil-example/projectile/lib/Drasil/Projectile/Assumptions.hs
29:twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
30:cartSyst        = cic "cartSyst"        cartSystDesc        "cartSyst"        assumpDom
31:yAxisGravity    = cic "yAxisGravity"    yAxisGravityDesc    "yAxisGravity"    assumpDom
32:launchOrigin    = cic "launchOrigin"    launchOriginDesc    "launchOrigin"    assumpDom
33:targetXAxis     = cic "targetXAxis"     targetXAxisDesc     "targetXAxis"     assumpDom
34:posXDirection   = cic "posXDirection"   posXDirectionDesc   "posXDirection"   assumpDom
35:constAccel      = cic "constAccel"      constAccelDesc      "constAccel"      assumpDom
36:accelXZero      = cic "accelXZero"      accelXZeroDesc      "accelXZero"      assumpDom
37:accelYGravity   = cic "accelYGravity"   accelYGravityDesc   "accelYGravity"   assumpDom
38:neglectDrag     = cic "neglectDrag"     neglectDragDesc     "neglectDrag"     assumpDom
39:pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
40:freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom
41:neglectCurv     = cic "neglectCurv"     neglectCurvDesc     "neglectCurv"     assumpDom
42:timeStartZero   = cic "timeStartZero"   timeStartZeroDesc   "timeStartZero"   assumpDom
43:gravAccelValue  = cic "gravAccelValue"  gravAccelValueDesc  "gravAccelValue"  assumpDom

drasil-example/dblpend/lib/Drasil/DblPend/Requirements.hs
25:verifyInptVals = cic "verifyInptVals" verifyInptValsDesc  "Verify-Input-Values"    funcReqDom
26:calcAng        = cic "calcAng"        calcAngDesc         "Calculate-Angle-Of-Rod" funcReqDom
27:outputValues   = cic "outputValues"   outputValuesDesc    "Output-Values"          funcReqDom

drasil-example/dblpend/lib/Drasil/DblPend/Assumptions.hs
26:twoDMotion        = cic "twoDMotion"    twoDMotionDesc          "twoDMotion"    assumpDom
27:cartSys           = cic "cartSys"       cartSysDesc             "cartSys"       assumpDom
28:cartSysR          = cic "cartSysR"      cartSysRDesc            "cartSysR"      assumpDom
29:yAxisDir          = cic "yAxisDir"      yAxisDirDesc            "yAxisDir"      assumpDom
30:startOriginSingle = cic "startOrigin"   startOriginDescSingle   "startOrigin"   assumpDom
31:startOriginDouble = cic "startOrigin"   startOriginDescDouble   "startOrigin"   assumpDom
32:firstPend         = cic "firstPend"     firstPendDesc           "firstPend"     assumpDom
33:secondPend        = cic "secondPend"    secondPendDesc          "secondPend"    assumpDom

drasil-example/glassbr/lib/Drasil/GlassBR/Changes.hs
27:calcInternalBlastRisk     = cic "calcInternalBlastRisk"     (calcInternalBlastRiskDesc blastRisk) "Calculate-Internal-Blast-Risk"       likeChgDom
28:varValsOfmkE              = cic "varValsOfmkE"              varValsOfmkEDesc                      "Variable-Values-of-m,k,E"            likeChgDom
29:accMoreThanSingleLite     = cic "accMoreThanSingleLite"     accMoreThanSingleLiteDesc             "Accomodate-More-than-Single-Lite"    likeChgDom
30:accMoreBoundaryConditions = cic "accMoreBoundaryConditions" accMoreBoundaryConditionsDesc         "Accomodate-More-Boundary-Conditions" likeChgDom
31:considerMoreThanFlexGlass = cic "considerMoreThanFlexGlass" considerMoreThanFlexGlassDesc         "Consider-More-than-Flexure-Glass"    likeChgDom
65:predictWithstandOfCertDeg = cic "predictWithstandOfCertDeg" predictWithstandOfCertDegDesc "Predict-Withstanding-of-Certain-Degree"  unlikeChgDom
66:accAlteredGlass           = cic "accAlteredGlass"           accAlteredGlassDesc           "Accommodate-Altered-Glass"               unlikeChgDom

drasil-example/gamephysics/lib/Drasil/GamePhysics/Changes.hs
40:lcVODES = cic "lcVODES" likelyChangesStmt1 "Variable-ODE-Solver" likeChgDom
41:lcEC = cic "lcEC" likelyChangesStmt2 "Expanded-Collisions" likeChgDom
42:lcID = cic "lcID" likelyChangesStmt3 "Include-Dampening" likeChgDom
43:lcIJC = cic "lcIJC" likelyChangesStmt4 "Include-Joints-Constraints" likeChgDom
61:ucSRB = cic "ucSRB" unlikelyChangesStmt1 "Simulate-Rigid-Bodies" unlikeChgDom
62:ucEI = cic "ucEI" unlikelyChangesStmt2 "External-Input" unlikeChgDom
63:ucCCS = cic "ucCCS" unlikelyChangesStmt3 "Cartesian-Coordinate-System" unlikeChgDom
64:ucORB = cic "ucORB" unlikelyChangesStmt4 "Objects-Rigid-Bodies" unlikeChgDom

drasil-example/gamephysics/lib/Drasil/GamePhysics/Requirements.hs
89:simSpace              = cic "simSpace"              simSpaceDesc              "Simulation-Space"                       funcReqDom
90:inputInitialConds     = cic "inputInitialConds"     inputInitialCondsDesc     "Input-Initial-Conditions"               funcReqDom
91:inputSurfaceProps     = cic "inputSurfaceProps"     inputSurfacePropsDesc     "Input-Surface-Properties"               funcReqDom
92:verifyPhysCons        = cic "verifyPhysCons"        verifyPhysConsDesc        "Verify-Physical_Constraints"            funcReqDom
93:calcTransOverTime     = cic "calcTransOverTime"     calcTransOverTimeDesc     "Calculate-Translation-Over-Time"        funcReqDom
94:calcRotOverTime       = cic "calcRotOverTime"       calcRotOverTimeDesc       "Calculate-Rotation-Over-Time"           funcReqDom
95:deterColls            = cic "deterColls"            deterCollsDesc            "Determine-Collisions"                   funcReqDom
96:deterCollRespOverTime = cic "deterCollRespOverTime" deterCollRespOverTimeDesc "Determine-Collision-Response-Over-Time" funcReqDom
106:performance = cic "performance" (foldlSent [
112:correctness = cic "correctness" (foldlSent [
123:usability = cic "usability" (foldlSent [
131:understandability = cic "understandability" (foldlSent [

drasil-example/glassbr/lib/Drasil/GlassBR/Requirements.hs
42:sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc "System-Set-Values-Following-Assumptions" funcReqDom
43:checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc     "Check-Input-with-Data_Constraints"       funcReqDom
44:outputValsAndKnownValues   = cic "outputValsAndKnownValues"   outputValsAndKnownValuesDesc   "Output-Values-and-Known-Values"          funcReqDom
45:checkGlassSafety           = cic "checkGlassSafety"           checkGlassSafetyDesc           "Check-Glass-Safety"                      funcReqDom
46:outputValues               = cic "outputValues"               outputValuesDesc               "Output-Values"                           funcReqDom

drasil-example/swhsnopcm/lib/Drasil/SWHSNoPCM/Assumptions.hs
46:assumpDWCoW = cic "assumpDWCoW" assumpS4
53:assumpSHECoW = cic "assumpSHECoW" assumpS5
62:assumpCTNTD = cic "assumpCTNTD" assumpS9_npcm
69:assumpNIHGBW = cic "assumpNIHGBW" assumpS12
72:assumpWAL = cic "assumpWAL" (assumpS14 $ phrase material_ +:+
81:assumpAPT = cic "assumpAPT" assumpS13

drasil-example/gamephysics/lib/Drasil/GamePhysics/Assumptions.hs
18:assumpOT = cic "assumpOT" (foldlSent assumpOTDesc) "objectTy" assumpDom
19:assumpOD = cic "assumpOD" (foldlSent assumpODDesc) "objectDimension" assumpDom
20:assumpCST = cic "assumpCST" (foldlSent assumpCSTDesc) "coordinateSystemTy" assumpDom
21:assumpAD = cic "assumpAD" (foldlSent assumpADDesc) "axesDefined" assumpDom
22:assumpCT = cic "assumpCT" (foldlSent assumpCTDesc) "collisionType" assumpDom
23:assumpDI = cic "assumpDI" (foldlSent assumpDIDesc) "dampingInvolvement" assumpDom
24:assumpCAJI = cic "assumpCAJI" (foldlSent assumpCAJIDesc) "constraintsAndJointsInvolvement" assumpDom

drasil-example/glassbr/lib/Drasil/GlassBR/Assumptions.hs
32:assumpGT           = cic "assumpGT"   glassTypeDesc                     "glassType"           Doc.assumpDom
33:assumpGC           = cic "assumpGC"   glassConditionDesc                "glassCondition"      Doc.assumpDom
34:assumpES           = cic "assumpES"   explainScenarioDesc               "explainScenario"     Doc.assumpDom
35:assumpSV           = cic "assumpSV"   (standardValuesDesc loadDur)      "standardValues"      Doc.assumpDom
36:assumpGL           = cic "assumpGL"   glassLiteDesc                     "glassLite"           Doc.assumpDom
37:assumpBC           = cic "assumpBC"   boundaryConditionsDesc            "boundaryConditions"  Doc.assumpDom
38:assumpRT           = cic "assumpRT"   responseTypeDesc                  "responseType"        Doc.assumpDom
39:assumpLDFC         = cic "assumpLDFC" (ldfConstantDesc lDurFac)         "ldfConstant"         Doc.assumpDom

drasil-example/ssp/lib/Drasil/SSP/Requirements.hs
38:readAndStore = cic "readAndStore" ( foldlSent [
43:verifyInput = cic "verifyInput" ( foldlSent [
48:determineCritSlip = cic "determineCritSlip" ( foldlSent [
56:verifyOutput = cic "verifyOutput" ( foldlSent [
61:displayInput = cic "displayInput" ( foldlSent [
66:displayGraph = cic "displayGraph" ( foldlSent [
71:displayFS = cic "displayFS" ( foldlSent [
76:displayNormal = cic "displayNormal" ( foldlSent [
80:displayShear = cic "displayShear" ( foldlSent [
84:writeToFile = cic "writeToFile" ( foldlSent [

drasil-example/ssp/lib/Drasil/SSP/Goals.hs
21:identifyCritAndFSGS = cic "identifyCritAndFS" identifyCritAndFS 
25:determineNormalFGS = cic "determineNormalF" (determineF intNormForce) 
29:determineShearFGS = cic "determineShearF" (determineF intShrForce) 

drasil-example/ssp/lib/Drasil/SSP/Assumptions.hs
31:assumpSSC = cic "assumpSSC" monotonicF "Slip-Surface-Concave" assumpDom
32:assumpFOSL = cic "assumpFOS" slopeS "Factor-of-Safety" assumpDom
33:assumpSLH = cic "assumpSLH" homogeneousL "Soil-Layer-Homogeneous" assumpDom
34:assumpSP = cic "assumpSP" propertiesS "Soil-Properties" assumpDom
35:assumpSLI = cic "assumpSLI" isotropicP "Soil-Layers-Isotropic" assumpDom
36:assumpINSFL = cic "assumpINSFL" linearS "Interslice-Norm-Shear-Forces-Linear" assumpDom
37:assumpPSC = cic "assumpPSC" planeS "Plane-Strain-Conditions" assumpDom
38:assumpENSL = cic "assumpENSL" largeN "Effective-Norm-Stress-Large" assumpDom
39:assumpSBSBISL = cic "assumpSBSBISL" straightS "Surface-Base-Slice-between-Interslice-Straight-Lines" assumpDom
40:assumpES = cic "assumpES" edgeS "Edge-Slices" assumpDom
41:assumpSF = cic "assumpSF" seismicF "Seismic-Force" assumpDom
42:assumpSL = cic "assumpSL" surfaceL "Surface-Load" assumpDom
43:assumpWIBE = cic "assumpWIBE" waterBIntersect "Water-Intersects-Base-Edge" 
45:assumpWISE = cic "assumpWISE" waterSIntersect "Water-Intersects-Surface-Edge" 
47:assumpNESSS = cic "assumpNESSS" negligibleSlopeEffect 
49:assumpHFSM = cic "assumpHFSM" hydrostaticFMidpoint 

drasil-example/ssp/lib/Drasil/SSP/Changes.hs
22:likelyChgCISL = cic "LC_inhomogeneous" lcCISLDesc "Calculate-Inhomogeneous-Soil-Layers" likeChgDom
25:likelyChgCSF = cic "LC_seismic" lcCSFDesc "Calculate-Seismic-Force" likeChgDom
28:likelyChgCEF = cic "LC_external" lcCEFDesc "Calculate-External-Force" likeChgDom
52:unlikelyChgNISLO = cic "UC_normshearlinear" ucNASLODesc "Normal-And-Shear-Linear-Only" unlikeChgDom
53:unlikelyChg2AO   = cic "UC_2donly"          uc2AODesc   "2D-Analysis-Only"             unlikeChgDom
```

</details>
