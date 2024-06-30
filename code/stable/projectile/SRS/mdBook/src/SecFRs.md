# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs).

<div id="verifyInVals"></div>

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](./SecDataConstraints.md#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="calcValues"></div>

Calculate-Values: Calculate the following values: \\({t\_{\text{flight}}}\\) (from [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime)), \\({p\_{\text{land}}}\\) (from [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist)), \\({d\_{\text{offset}}}\\) (from [IM:offsetIM](./SecIMs.md#IM:offsetIM)), and \\(s\\) (from [IM:messageIM](./SecIMs.md#IM:messageIM)).

<div id="outputValues"></div>

Output-Values: Output \\({t\_{\text{flight}}}\\) (from [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime)), \\(s\\) (from [IM:messageIM](./SecIMs.md#IM:messageIM)), and \\({d\_{\text{offset}}}\\) (from [IM:offsetIM](./SecIMs.md#IM:offsetIM)).


<div id="Table:ReqInputs"></div>

|Symbol                    |Description    |Units                          |
|:-------------------------|:--------------|:------------------------------|
|\\({p\_{\text{target}}}\\)|Target position|\\({\text{m}}\\)               |
|\\({v\_{\text{launch}}}\\)|Launch speed   |\\(\frac{\text{m}}{\text{s}}\\)|
|\\(θ\\)                   |Launch angle   |\\({\text{rad}}\\)             |

**<p align="center">Required Inputs following [FR:Input-Values](./SecFRs.md#inputValues)</p>**

