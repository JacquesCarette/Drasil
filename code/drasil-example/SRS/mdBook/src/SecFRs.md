# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs).

<div id="verifyInptVals"></div>

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](./SecDataConstraints.md#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="calcAng"></div>

Calculate-Angle-Of-Rod: Calculate the following values: \\({θ\_{1}}\\) and \\({θ\_{2}}\\) (from [IM:calOfAngle1](./SecIMs.md#IM:calOfAngle1) and [IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)).

<div id="outputValues"></div>

Output-Values: Output \\({θ\_{1}}\\) and \\({θ\_{2}}\\) (from [IM:calOfAngle1](./SecIMs.md#IM:calOfAngle1) and [IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)).

<div id="Table:ReqInputs"></div>

|Symbol        |Description              |Units             |
|:-------------|:------------------------|:-----------------|
|\\({L\_{1}}\\)|Length of the first rod  |\\({\text{m}}\\)  |
|\\({L\_{2}}\\)|Length of the second rod |\\({\text{m}}\\)  |
|\\({m\_{1}}\\)|Mass of the first object |\\({\text{kg}}\\) |
|\\({m\_{2}}\\)|Mass of the second object|\\({\text{kg}}\\) |
|\\({θ\_{1}}\\)|Angle of the first rod   |\\({\text{rad}}\\)|
|\\({θ\_{2}}\\)|Angle of the second rod  |\\({\text{rad}}\\)|

**<p align="center">Required Inputs</p>**
