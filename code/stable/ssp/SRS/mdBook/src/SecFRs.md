# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="readAndStore"></div>

Read-and-Store: Read the inputs, shown in the table [Required Inputs](./SecFRs.md#Table:ReqInputs), and store the data.

<div id="verifyInput"></div>

Verify-Input: Verify that the input data lie within the [physical constraints](./SecDataConstraints.md#Sec:DataConstraints).

<div id="determineCritSlip"></div>

Determine-Critical-Slip-Surface: Determine the critical slip surface for the input slope, corresponding to the minimum factor of safety, by using [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [IM:intsliceFs](./SecIMs.md#IM:intsliceFs) to calculate the factor of safety for a slip surface and using [IM:crtSlpId](./SecIMs.md#IM:crtSlpId) to find the slip surface that minimizes it.

<div id="verifyOutput"></div>

Verify-Output: Verify that the minimum factor of safety and critical slip surface satisfy the physical constraints shown in [Properties of a Correct Solution](./SecCorSolProps.md#Sec:CorSolProps).

<div id="displayInput"></div>

Display-Input: Display as output the user-supplied inputs listed in [Tab:inputsToOutputTable](./SecFRs.md#Table:inputsToOutputTable).

<div id="displayGraph"></div>

Display-Graph: Display the critical slip surface of the 2D slope, as determined from [IM:crtSlpId](./SecIMs.md#IM:crtSlpId), graphically.

<div id="displayFS"></div>

Display-Factor-of-Safety: Display the value of the factor of safety for the critical slip surface, as determined from [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [IM:intsliceFs](./SecIMs.md#IM:intsliceFs).

<div id="displayNormal"></div>

Display-Interslice-Normal-Forces: Using [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [IM:intsliceFs](./SecIMs.md#IM:intsliceFs), calculate and graphically display the interslice normal forces.

<div id="displayShear"></div>

Display-Interslice-Shear-Forces: Using [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [IM:intsliceFs](./SecIMs.md#IM:intsliceFs), calculate and graphically display the interslice shear forces.

<div id="writeToFile"></div>

Write-Results-To-File: Provide the option of writing the output result data, as given in [FR:Display-Input](./SecFRs.md#displayInput), [FR:Display-Graph](./SecFRs.md#displayGraph), [FR:Display-Factor-of-Safety](./SecFRs.md#displayFS), [FR:Display-Interslice-Normal-Forces](./SecFRs.md#displayNormal), and [FR:Display-Interslice-Shear-Forces](./SecFRs.md#displayShear), to a file.

<div id="Table:ReqInputs"></div>

|Symbol                                |Description                           |Units                              |
|:-------------------------------------|:-------------------------------------|:----------------------------------|
|\\(\text{(x,y)}\\)                    |Cartesian position coordinates        |\\({\text{m}}\\)                   |
|\\(c'\\)                              |Effective cohesion                    |\\({\text{Pa}}\\)                  |
|\\(\mathit{const\_f}\\)               |Decision on f                         |--                                 |
|\\(x^{\text{maxEtr}}\_{\text{slip}}\\)|Maximum entry \\(x\\)-coordinate      |\\({\text{m}}\\)                   |
|\\(x^{\text{maxExt}}\_{\text{slip}}\\)|Maximum exit \\(x\\)-coordinate       |\\({\text{m}}\\)                   |
|\\(x^{\text{minEtr}}\_{\text{slip}}\\)|Minimum entry \\(x\\)-coordinate      |\\({\text{m}}\\)                   |
|\\(x^{\text{minExt}}\_{\text{slip}}\\)|Minimum exit \\(x\\)-coordinate       |\\({\text{m}}\\)                   |
|\\(\boldsymbol{x}\_{\text{slope}}\\)  |\\(x\\)-coordinates of the slope      |\\({\text{m}}\\)                   |
|\\(\boldsymbol{x}\_{\text{wt}}\\)     |\\(x\\)-coordinates of the water table|\\({\text{m}}\\)                   |
|\\(y^{\text{max}}\_{\text{slip}}\\)   |Maximum \\(y\\)-coordinate            |\\({\text{m}}\\)                   |
|\\(y^{\text{min}}\_{\text{slip}}\\)   |Minimum \\(y\\)-coordinate            |\\({\text{m}}\\)                   |
|\\(\boldsymbol{y}\_{\text{slope}}\\)  |\\(y\\)-coordinates of the slope      |\\({\text{m}}\\)                   |
|\\(\boldsymbol{y}\_{\text{wt}}\\)     |\\(y\\)-coordinates of the water table|\\({\text{m}}\\)                   |
|\\(γ\_{\text{dry}}\\)                 |Soil dry unit weight                  |\\(\frac{\text{N}}{\text{m}^{3}}\\)|
|\\(γ\_{\text{sat}}\\)                 |Soil saturated unit weight            |\\(\frac{\text{N}}{\text{m}^{3}}\\)|
|\\(γ\_{w}\\)                          |Unit weight of water                  |\\(\frac{\text{N}}{\text{m}^{3}}\\)|
|\\(φ'\\)                              |Effective angle of friction           |\\({{}^{\circ}}\\)                 |

**<p align="center">Required Inputs</p>**

<div id="Table:inputsToOutputTable"></div>

|Symbol                                |Name                            |
|:-------------------------------------|:-------------------------------|
|\\(\mathit{const\_f}\\)               |decision on f                   |
|\\(x^{\text{maxExt}}\_{\text{slip}}\\)|maximum exit \\(x\\)-coordinate |
|\\(x^{\text{maxEtr}}\_{\text{slip}}\\)|maximum entry \\(x\\)-coordinate|
|\\(x^{\text{minExt}}\_{\text{slip}}\\)|minimum exit \\(x\\)-coordinate |
|\\(x^{\text{minEtr}}\_{\text{slip}}\\)|minimum entry \\(x\\)-coordinate|
|\\(y^{\text{max}}\_{\text{slip}}\\)   |maximum \\(y\\)-coordinate      |
|\\(y^{\text{min}}\_{\text{slip}}\\)   |minimum \\(y\\)-coordinate      |

**<p align="center">Inputs to be Returned as Output</p>**
