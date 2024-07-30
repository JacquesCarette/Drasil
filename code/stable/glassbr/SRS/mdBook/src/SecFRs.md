# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs), which define the glass dimensions, type of glass, tolerable probability of failure, and the characteristics of the blast.

<div id="sysSetValsFollowingAssumps"></div>

System-Set-Values-Following-Assumptions: The system shall set the known values as described in the table for [Required Assignments](./SecFRs.md#Table:ReqAssignments).

<div id="checkInputWithDataCons"></div>

Check-Input-with-Data_Constraints: The system shall check the entered input values to ensure that they do not exceed the [data constraints](./SecDataConstraints.md#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="outputValsAndKnownValues"></div>

Output-Values-and-Known-Values: Output the input values from [FR:Input-Values](./SecFRs.md#inputValues) and the known values from [FR:System-Set-Values-Following-Assumptions](./SecFRs.md#sysSetValsFollowingAssumps).

<div id="checkGlassSafety"></div>

Check-Glass-Safety: If \\(\mathit{isSafePb}\land{}\mathit{isSafeLR}\\) (from [IM:isSafePb](./SecIMs.md#IM:isSafePb) and [IM:isSafeLR](./SecIMs.md#IM:isSafeLR)), output the message "For the given input parameters, the glass is considered safe." If the condition is false, then output the message "For the given input parameters, the glass is NOT considered safe."

<div id="outputValues"></div>

Output-Values: Output the values from the table for [Required Outputs](./SecFRs.md#Table:ReqOutputs).

<div id="Table:ReqInputs"></div>

|Symbol                         |Description                                                                           |Units            |
|:------------------------------|:-------------------------------------------------------------------------------------|:----------------|
|\\(a\\)                        |Plate length (long dimension)                                                         |\\({\text{m}}\\) |
|\\(b\\)                        |Plate width (short dimension)                                                         |\\({\text{m}}\\) |
|\\(g\\)                        |Glass type                                                                            |--               |
|\\({P\_{\text{b}\text{tol}}}\\)|Tolerable probability of breakage                                                     |--               |
|\\({\mathit{SD}\_{\text{x}}}\\)|Stand off distance (\\(x\\)-component)                                                |\\({\text{m}}\\) |
|\\({\mathit{SD}\_{\text{y}}}\\)|Stand off distance (\\(y\\)-component)                                                |\\({\text{m}}\\) |
|\\({\mathit{SD}\_{\text{z}}}\\)|Stand off distance (\\(z\\)-component)                                                |\\({\text{m}}\\) |
|\\(t\\)                        |Nominal thickness \\(t\in{}\{2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0\}\\)|\\({\text{mm}}\\)|
|\\(\mathit{TNT}\\)             |TNT equivalent factor                                                                 |--               |
|\\(w\\)                        |Charge weight                                                                         |\\({\text{kg}}\\)|

**<p align="center">Required Inputs following [FR:Input-Values](./SecFRs.md#inputValues)</p>**

<div id="Table:ReqAssignments"></div>

|Symbol               |Description                   |Source                                          |Units                                   |
|:--------------------|:-----------------------------|:-----------------------------------------------|:---------------------------------------|
|\\(\mathit{AR}\\)    |Aspect ratio                  |[DD:aspectRatio](./SecDDs.md#DD:aspectRatio)    |--                                      |
|\\(E\\)              |Modulus of elasticity of glass|[A:standardValues](./SecAssumps.md#assumpSV)    |\\({\text{Pa}}\\)                       |
|\\(\mathit{GTF}\\)   |Glass type factor             |[DD:gTF](./SecDDs.md#DD:gTF)                    |--                                      |
|\\(h\\)              |Minimum thickness             |[DD:minThick](./SecDDs.md#DD:minThick)          |\\({\text{m}}\\)                        |
|\\(k\\)              |Surface flaw parameter        |[A:standardValues](./SecAssumps.md#assumpSV)    |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{LDF}\\)   |Load duration factor          |[DD:loadDurFactor](./SecDDs.md#DD:loadDurFactor)|--                                      |
|\\(\mathit{LSF}\\)   |Load share factor             |[A:glassLite](./SecAssumps.md#assumpGL)         |--                                      |
|\\(m\\)              |Surface flaw parameter        |[A:standardValues](./SecAssumps.md#assumpSV)    |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{SD}\\)    |Stand off distance            |[DD:standOffDist](./SecDDs.md#DD:standOffDist)  |\\({\text{m}}\\)                        |
|\\({t\_{\text{d}}}\\)|Duration of load              |[A:standardValues](./SecAssumps.md#assumpSV)    |\\({\text{s}}\\)                        |

**<p align="center">Required Assignments following [FR:System-Set-Values-Following-Assumptions](./SecFRs.md#sysSetValsFollowingAssumps)</p>**

<div id="Table:ReqOutputs"></div>

|Symbol                       |Description                                           |Source                                          |Units            |
|:----------------------------|:-----------------------------------------------------|:-----------------------------------------------|:----------------|
|\\(\mathit{AR}\\)            |Aspect ratio                                          |[DD:aspectRatio](./SecDDs.md#DD:aspectRatio)    |--               |
|\\(B\\)                      |Risk of failure                                       |[IM:riskFun](./SecIMs.md#IM:riskFun)            |--               |
|\\(\mathit{GTF}\\)           |Glass type factor                                     |[DD:gTF](./SecDDs.md#DD:gTF)                    |--               |
|\\(h\\)                      |Minimum thickness                                     |[DD:minThick](./SecDDs.md#DD:minThick)          |\\({\text{m}}\\) |
|\\(\mathit{isSafeLR}\\)      |3 second load equivalent resistance safety requirement|[IM:isSafeLR](./SecIMs.md#IM:isSafeLR)          |--               |
|\\(\mathit{isSafePb}\\)      |Probability of glass breakage safety requirement      |[IM:isSafePb](./SecIMs.md#IM:isSafePb)          |--               |
|\\(J\\)                      |Stress distribution factor (Function)                 |[IM:stressDistFac](./SecIMs.md#IM:stressDistFac)|--               |
|\\({J\_{\text{tol}}}\\)      |Tolerable stress distribution factor                  |[IM:sdfTol](./SecIMs.md#IM:sdfTol)              |--               |
|\\(\mathit{LR}\\)            |Load resistance                                       |[IM:calofCapacity](./SecIMs.md#IM:calofCapacity)|\\({\text{Pa}}\\)|
|\\(\mathit{NFL}\\)           |Non-factored load                                     |[IM:nFL](./SecIMs.md#IM:nFL)                    |\\({\text{Pa}}\\)|
|\\({P\_{\text{b}}}\\)        |Probability of breakage                               |[IM:probOfBreak](./SecIMs.md#IM:probOfBreak)    |--               |
|\\(\hat{q}\\)                |Dimensionless load                                    |[IM:dimlessLoad](./SecIMs.md#IM:dimlessLoad)    |--               |
|\\({\hat{q}\_{\text{tol}}}\\)|Tolerable load                                        |[IM:tolLoad](./SecIMs.md#IM:tolLoad)            |--               |

**<p align="center">Required Outputs following [FR:Output-Values](./SecFRs.md#outputValues)</p>**
