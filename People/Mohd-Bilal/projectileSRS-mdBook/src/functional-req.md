# Functional Requirements

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues">

Input-Values: Input the values from [Tab:ReqInputs](./functional-req.md#Table:ReqInputs).

</div>

<div id="verifyInVals">

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](./data-constraints.md). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

</div>

<div id="calcValues">

Calculate-Values: Calculate the following values: \\({t_{\text{flight}}}\\) (from [IM:calOfLandingTime](./instance-models.md#IM:calOfLandingTime)), \\({p_{\text{land}}}\\) (from [IM:calOfLandingDist](./instance-models.md#IM:calOfLandingDist)), \\({d_{\text{offset}}}\\) (from [IM:offsetIM](./instance-models.md#IM:offsetIM)), and \\(s\\) (from [IM:messageIM](./instance-models.md#IM:messageIM)).

</div>

<div id="outputValues">

Output-Values: Output \\({t_{\text{flight}}}\\) (from [IM:calOfLandingTime](./instance-models.md#IM:calOfLandingTime)), \\(s\\) (from [IM:messageIM](./instance-models.md#IM:messageIM)), and \\({d_{\text{offset}}}\\) (from [IM:offsetIM](./instance-models.md#IM:offsetIM)).

</div>

<div id="Table:ReqInputs">

|Symbol|Description|Units|
|-|-|-|
|\\({p_{\text{target}}}\\)|Target position|\\({\text{m}}\\)|
|\\({v_{\text{launch}}}\\)|Launch speed|\\(\frac{\text{m}}{\text{s}}\\)|
|\\(θ\\)|Launch angle|\\({\text{rad}}\\)|

**<p align="center">Required Inputs following <a href="./functional-req.md#inputValues">FR:Input-Values</a></p>**

</div>
