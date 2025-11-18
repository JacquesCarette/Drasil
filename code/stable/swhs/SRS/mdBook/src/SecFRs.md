# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](./SecFRs.md#Table:ReqInputs), which define the tank parameters, material properties, and initial conditions.

<div id="findMass"></div>

Find-Mass: Use the inputs in [FR:Input-Values](./SecFRs.md#inputValues) to find the masses needed for [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr), [IM:eBalanceOnPCM](./SecIMs.md#IM:eBalanceOnPCM), [IM:heatEInWtr](./SecIMs.md#IM:heatEInWtr), and [IM:heatEInPCM](./SecIMs.md#IM:heatEInPCM), using [DD:waterMass](./SecDDs.md#DD:waterMass), [DD:waterVolume_pcm](./SecDDs.md#DD:waterVolume.pcm), and [DD:tankVolume](./SecDDs.md#DD:tankVolume).

<div id="checkWithPhysConsts"></div>

Check-Input-with-Physical_Constraints: Verify that the inputs satisfy the required [physical constraints](./SecDataConstraints.md#Sec:DataConstraints).

<div id="outputInputDerivVals"></div>

Output-Input-Derived-Values: Output the input values and derived values in the following list: the values (from [FR:Input-Values](./SecFRs.md#inputValues)), the masses (from [FR:Find-Mass](./SecFRs.md#findMass)), \\({τ\_{\text{W}}}\\) (from [DD:balanceDecayRate](./SecDDs.md#DD:balanceDecayRate)), \\(η\\) (from [DD:balanceDecayTime](./SecDDs.md#DD:balanceDecayTime)), \\({{τ\_{\text{P}}}^{\text{S}}}\\) (from [DD:balanceSolidPCM](./SecDDs.md#DD:balanceSolidPCM)), and \\({{τ\_{\text{P}}}^{\text{L}}}\\) (from [DD:balanceLiquidPCM](./SecDDs.md#DD:balanceLiquidPCM)).

<div id="calcValues"></div>

Calculate-Values: Calculate the following values: \\({T\_{\text{W}}}\\)(\\(t\\)) (from [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr)), \\({T\_{\text{P}}}\\)(\\(t\\)) (from [IM:eBalanceOnPCM](./SecIMs.md#IM:eBalanceOnPCM)), \\({E\_{\text{W}}}\\)(\\(t\\)) (from [IM:heatEInWtr](./SecIMs.md#IM:heatEInWtr)), and \\({E\_{\text{P}}}\\)(\\(t\\)) (from [IM:heatEInPCM](./SecIMs.md#IM:heatEInPCM)).

<div id="verifyEnergyOutput"></div>

Verify-Energy-Output-Follow-Conservation-of-Energy: Verify that the energy outputs (\\({E\_{\text{W}}}\\)(\\(t\\)) and \\({E\_{\text{P}}}\\)(\\(t\\))) follow the law of conservation of energy, as outlined in [Properties of a Correct Solution](./SecCorSolProps.md#Sec:CorSolProps), with relative error no greater than \\({C\_{\text{tol}}}\\).

<div id="calcPCMMeltBegin"></div>

Calculate-PCM-Melt-Begin-Time: Calculate and output the time at which the PCM begins to melt \\({{t\_{\text{melt}}}^{\text{init}}}\\) (from [IM:eBalanceOnPCM](./SecIMs.md#IM:eBalanceOnPCM)).

<div id="calcPCMMeltEnd"></div>

Calculate-PCM-Melt-End-Time: Calculate and output the time at which the PCM stops melting \\({{t\_{\text{melt}}}^{\text{final}}}\\) (from [IM:eBalanceOnPCM](./SecIMs.md#IM:eBalanceOnPCM)).

<div id="outputValues"></div>

Output-Values: Output \\({T\_{\text{W}}}\\)(\\(t\\)) (from [IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr)), \\({T\_{\text{P}}}\\)(\\(t\\)) (from [IM:eBalanceOnPCM](./SecIMs.md#IM:eBalanceOnPCM)), \\({E\_{\text{W}}}\\)(\\(t\\)) (from [IM:heatEInWtr](./SecIMs.md#IM:heatEInWtr)), and \\({E\_{\text{P}}}\\)(\\(t\\)) (from [IM:heatEInPCM](./SecIMs.md#IM:heatEInPCM)).

<div id="Table:ReqInputs"></div>

|Symbol                               |Description                                                |Units                                                |
|:------------------------------------|:----------------------------------------------------------|:----------------------------------------------------|
|\\({A\_{\text{C}}}\\)                |Heating coil surface area                                  |\\({\text{m}^{2}}\\)                                 |
|\\({A\_{\text{P}}}\\)                |Phase change material surface area                         |\\({\text{m}^{2}}\\)                                 |
|\\({A\_{\text{tol}}}\\)              |Absolute tolerance                                         |--                                                   |
|\\({{C\_{\text{P}}}^{\text{L}}}\\)   |Specific heat capacity of PCM as a liquid                  |\\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\)   |
|\\({{C\_{\text{P}}}^{\text{S}}}\\)   |Specific heat capacity of PCM as a solid                   |\\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\)   |
|\\({C\_{\text{W}}}\\)                |Specific heat capacity of water                            |\\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\)   |
|\\(D\\)                              |Diameter of tank                                           |\\({\text{m}}\\)                                     |
|\\({H\_{\text{f}}}\\)                |Specific latent heat of fusion                             |\\(\frac{\text{J}}{\text{kg}}\\)                     |
|\\({h\_{\text{C}}}\\)                |Convective heat transfer coefficient between coil and water|\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|
|\\({h\_{\text{P}}}\\)                |Convective heat transfer coefficient between PCM and water |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|
|\\(L\\)                              |Length of tank                                             |\\({\text{m}}\\)                                     |
|\\({R\_{\text{tol}}}\\)              |Relative tolerance                                         |--                                                   |
|\\({T\_{\text{C}}}\\)                |Temperature of the heating coil                            |\\({{}^{\circ}\text{C}}\\)                           |
|\\({T\_{\text{init}}}\\)             |Initial temperature                                        |\\({{}^{\circ}\text{C}}\\)                           |
|\\({{T\_{\text{melt}}}^{\text{P}}}\\)|Melting point temperature for PCM                          |\\({{}^{\circ}\text{C}}\\)                           |
|\\({t\_{\text{final}}}\\)            |Final time                                                 |\\({\text{s}}\\)                                     |
|\\({t\_{\text{step}}}\\)             |Time step for simulation                                   |\\({\text{s}}\\)                                     |
|\\({V\_{\text{P}}}\\)                |Volume of PCM                                              |\\({\text{m}^{3}}\\)                                 |
|\\({ρ\_{\text{P}}}\\)                |Density of PCM                                             |\\(\frac{\text{kg}}{\text{m}^{3}}\\)                 |
|\\({ρ\_{\text{W}}}\\)                |Density of water                                           |\\(\frac{\text{kg}}{\text{m}^{3}}\\)                 |

**<p align="center">Required Inputs</p>**
