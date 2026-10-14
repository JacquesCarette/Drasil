# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs).

<div id="verifyInptVals"></div>

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](./SecDataConstraints.md#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the computation stops.

<div id="calcPositions"></div>

Calculate-Positions: Calculate the positions of both stars over the simulation interval by solving [IM:accelX1](./SecIMs.md#IM:accelX1), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2) and [IM:accelY2](./SecIMs.md#IM:accelY2).

<div id="verifyOutput"></div>

Verify-Output: Verify that the computed results satisfy the conservation of total mechanical energy within a specified numerical tolerance.

<div id="outputValues"></div>

Output-Values: Output \\({x\_{1}}\\), \\({y\_{1}}\\), \\({x\_{2}}\\) and \\({y\_{2}}\\) from [IM:accelX1](./SecIMs.md#IM:accelX1), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2) and [IM:accelY2](./SecIMs.md#IM:accelY2).

<div id="Table:ReqInputs"></div>

|Symbol                      |Description                          |Units                          |
|:---------------------------|:------------------------------------|:------------------------------|
|\\({m\_{1}}\\)              |Mass of the first star               |\\({\text{kg}}\\)              |
|\\({m\_{2}}\\)              |Mass of the second star              |\\({\text{kg}}\\)              |
|\\({t\_{\text{final}}}\\)   |Final time                           |\\({\text{s}}\\)               |
|\\({{v\_{\text{x}1}}^{0}}\\)|Initial x-velocity of the first star |\\(\frac{\text{m}}{\text{s}}\\)|
|\\({{v\_{\text{x}2}}^{0}}\\)|Initial x-velocity of the second star|\\(\frac{\text{m}}{\text{s}}\\)|
|\\({{v\_{\text{y}1}}^{0}}\\)|Initial y-velocity of the first star |\\(\frac{\text{m}}{\text{s}}\\)|
|\\({{v\_{\text{y}2}}^{0}}\\)|Initial y-velocity of the second star|\\(\frac{\text{m}}{\text{s}}\\)|
|\\({{x\_{1}}^{0}}\\)        |Initial x-position of the first star |\\({\text{m}}\\)               |
|\\({{x\_{2}}^{0}}\\)        |Initial x-position of the second star|\\({\text{m}}\\)               |
|\\({{y\_{1}}^{0}}\\)        |Initial y-position of the first star |\\({\text{m}}\\)               |
|\\({{y\_{2}}^{0}}\\)        |Initial y-position of the second star|\\({\text{m}}\\)               |

**<p align="center">Required Inputs</p>**
