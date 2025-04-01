# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs).

<div id="verifyInptVals"></div>

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](./SecDataConstraints.md#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="calcAngPos"></div>

Calculate-Angular-Position-Of-Mass: Calculate the following values: \\(θ\\) and \\(θ\_{p}\\) (from [IM:calOfAngularDisplacement](./SecIMs.md#IM:calOfAngularDisplacement)).

<div id="outputValues"></div>

Output-Values: Output \\(L\_{\text{rod}}\\) (from [IM:calOfAngularDisplacement](./SecIMs.md#IM:calOfAngularDisplacement)).

<div id="Table:ReqInputs"></div>

|Symbol               |Description                       |Units                                |
|:--------------------|:---------------------------------|:------------------------------------|
|\\(L\_{\text{rod}}\\)|Length of the rod                 |\\({\text{m}}\\)                     |
|\\(m\\)              |Mass                              |\\({\text{kg}}\\)                    |
|\\(α\\)              |Angular acceleration              |\\(\frac{\text{rad}}{\text{s}^{2}}\\)|
|\\(θ\_{i}\\)         |Initial pendulum angle            |\\({\text{rad}}\\)                   |
|\\(θ\_{p}\\)         |Displacement angle of the pendulum|\\({\text{rad}}\\)                   |

**<p align="center">Required Inputs</p>**
