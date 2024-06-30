# Properties of a Correct Solution {#Sec:CorSolProps}

The [Data Constraints Table](./SecCorSolProps.md#Table:OutDataConstraints) shows the data constraints on the output variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable.

<div id="Table:OutDataConstraints"></div>

|Var                  |Physical Constraints                                              |
|:--------------------|:-----------------------------------------------------------------|
|\\({T\_{\text{W}}}\\)|\\({T\_{\text{init}}}\leq{}{T\_{\text{W}}}\leq{}{T\_{\text{C}}}\\)|
|\\({T\_{\text{P}}}\\)|\\({T\_{\text{init}}}\leq{}{T\_{\text{P}}}\leq{}{T\_{\text{C}}}\\)|
|\\({E\_{\text{W}}}\\)|\\({E\_{\text{W}}}\geq{}0\\)                                      |
|\\({E\_{\text{P}}}\\)|\\({E\_{\text{P}}}\geq{}0\\)                                      |

**<p align="center">Output Data Constraints</p>**

A correct solution must exhibit the law of conservation of energy. This means that the change in heat energy in the water should equal the difference between the total energy input from the heating coil and the energy output to the PCM. This can be shown as an equation by taking [GD:htFluxWaterFromCoil](./SecGDs.md#GD:htFluxWaterFromCoil) and [GD:htFluxPCMFromWater](./SecGDs.md#GD:htFluxPCMFromWater), multiplying each by their respective surface area of heat transfer, and integrating each over the simulation time, as follows:

\\[{E\_{\text{W}}}=\int\_{0}^{t}{{h\_{\text{C}}} {A\_{\text{C}}} \left({T\_{\text{C}}}-{T\_{\text{W}}}\left(t\right)\right)}\\,dt-\int\_{0}^{t}{{h\_{\text{P}}} {A\_{\text{P}}} \left({T\_{\text{W}}}\left(t\right)-{T\_{\text{P}}}\left(t\right)\right)}\\,dt\\]

In addition, the change in heat energy in the PCM should equal the energy input to the PCM from the water. This can be expressed as

\\[{E\_{\text{P}}}=\int\_{0}^{t}{{h\_{\text{P}}} {A\_{\text{P}}} \left({T\_{\text{W}}}\left(t\right)-{T\_{\text{P}}}\left(t\right)\right)}\\,dt\\]

Equations (FIXME: Equation 7) and (FIXME: Equation 8) can be used as "sanity" checks to gain confidence in any solution computed by SWHS. The relative error between the results computed by SWHS and the results calculated from the RHS of these equations should be less than \\({C\_{\text{tol}}}\\) [FR:Verify-Energy-Output-Follow-Conservation-of-Energy](./SecFRs.md#verifyEnergyOutput).

