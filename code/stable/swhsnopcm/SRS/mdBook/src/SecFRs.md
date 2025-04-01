# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs), which define the tank parameters, material properties, and initial conditions.

<div id="findMass"></div>

Find-Mass: Use the inputs in [FR:Input-Values](./SecFRs.md#inputValues) to find the mass needed for [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr), using [DD:waterMass](./SecDDs.md#DD:waterMass), [DD:waterVolume_nopcm](./SecDDs.md#DD:waterVolume.nopcm), and [DD:tankVolume](./SecDDs.md#DD:tankVolume).

<div id="checkWithPhysConsts"></div>

Check-Input-with-Physical_Constraints: Verify that the inputs satisfy the required [physical constraints](./SecDataConstraints.md#Sec:DataConstraints).

<div id="outputInputDerivVals"></div>

Output-Input-Derived-Values: Output the input values and derived values in the following list: the values (from [FR:Input-Values](./SecFRs.md#inputValues)), the mass (from [FR:Find-Mass](./SecFRs.md#findMass)), and \\(τ\_{\text{W}}\\) (from [DD:balanceDecayRate](./SecDDs.md#DD:balanceDecayRate)).

<div id="calcValues"></div>

Calculate-Values: Calculate the following values: \\(T\_{\text{W}}\\)(\\(t\\)) (from [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr)) and \\(E\_{\text{W}}\\)(\\(t\\)) (from [IM:heatEInWtr](./SecIMs.md#IM:heatEInWtr)).

<div id="outputValues"></div>

Output-Values: Output \\(T\_{\text{W}}\\)(\\(t\\)) (from [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr)) and \\(E\_{\text{W}}\\)(\\(t\\)) (from [IM:heatEInWtr](./SecIMs.md#IM:heatEInWtr)).

<div id="Table:ReqInputs"></div>

|Symbol                 |Description                                                |Units                                                |
|:----------------------|:----------------------------------------------------------|:----------------------------------------------------|
|\\(A\_{\text{C}}\\)    |Heating coil surface area                                  |\\({\text{m}^{2}}\\)                                 |
|\\(A\_{\text{tol}}\\)  |Absolute tolerance                                         |--                                                   |
|\\(C\_{\text{W}}\\)    |Specific heat capacity of water                            |\\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\)   |
|\\(D\\)                |Diameter of tank                                           |\\({\text{m}}\\)                                     |
|\\(E\_{\text{W}}\\)    |Change in heat energy in the water                         |\\({\text{J}}\\)                                     |
|\\(h\_{\text{C}}\\)    |Convective heat transfer coefficient between coil and water|\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|
|\\(L\\)                |Length of tank                                             |\\({\text{m}}\\)                                     |
|\\(R\_{\text{tol}}\\)  |Relative tolerance                                         |--                                                   |
|\\(T\_{\text{C}}\\)    |Temperature of the heating coil                            |\\({{}^{\circ}\text{C}}\\)                           |
|\\(T\_{\text{init}}\\) |Initial temperature                                        |\\({{}^{\circ}\text{C}}\\)                           |
|\\(t\_{\text{final}}\\)|Final time                                                 |\\({\text{s}}\\)                                     |
|\\(t\_{\text{step}}\\) |Time step for simulation                                   |\\({\text{s}}\\)                                     |
|\\(ρ\_{\text{W}}\\)    |Density of water                                           |\\(\frac{\text{kg}}{\text{m}^{3}}\\)                 |

**<p align="center">Required Inputs</p>**
