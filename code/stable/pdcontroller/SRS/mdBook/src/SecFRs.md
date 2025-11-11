# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs), which define the tunable controller parameters.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs), which define the tunable controller parameters.

<div id="verifyInputs"></div>

Verify-Input-Values: Ensure that the input values are within the limits specified in the [data constraints](./SecDataConstraints.md#Sec:DataConstraints).

<div id="calculateValues"></div>

Calculate-Values: Calculate the Process Variable (from [IM:pdEquationIM](./SecIMs.md#IM:pdEquationIM)) over the simulation time.

<div id="outputValues"></div>

Output-Values: Output the Process Variable (from [IM:pdEquationIM](./SecIMs.md#IM:pdEquationIM)) over the simulation time.

<div id="Table:ReqInputs"></div>

|Symbol                  |Description      |Units           |
|:-----------------------|:----------------|:---------------|
|\\({K\_{\text{d}}}\\)   |Derivative Gain  |--              |
|\\({K\_{\text{p}}}\\)   |Proportional Gain|--              |
|\\({r\_{\text{t}}}\\)   |Set-Point        |--              |
|\\({t\_{\text{sim}}}\\) |Simulation Time  |\\({\text{s}}\\)|
|\\({t\_{\text{step}}}\\)|Step Time        |\\({\text{s}}\\)|

**<p align="center">Required Inputs</p>**
