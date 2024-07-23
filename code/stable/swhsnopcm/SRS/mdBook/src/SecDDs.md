# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Mass of water {#DD:waterMass}

</div>

|Refname    |DD:waterMass                                                                                                                                                                                                                                             |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Mass of water                                                                                                                                                                                                                                            |
|Symbol     |\\({m\_{\text{W}}}\\)                                                                                                                                                                                                                                    |
|Units      |\\({\text{kg}}\\)                                                                                                                                                                                                                                        |
|Equation   |\\[{m\_{\text{W}}}={V\_{\text{W}}} {ρ\_{\text{W}}}\\]                                                                                                                                                                                                    |
|Description|<ul><li>\\({m\_{\text{W}}}\\) is the mass of water (\\({\text{kg}}\\))</li><li>\\({V\_{\text{W}}}\\) is the volume of water (\\({\text{m}^{3}}\\))</li><li>\\({ρ\_{\text{W}}}\\) is the density of water (\\(\frac{\text{kg}}{\text{m}^{3}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                       |
|RefBy      |[FR:Find-Mass](./SecFRs.md#findMass)                                                                                                                                                                                                                     |

<div align="center">

## Volume of water {#DD:waterVolume.nopcm}

</div>

|Refname    |DD:waterVolume.nopcm                                                                                                                                                               |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Volume of water                                                                                                                                                                    |
|Symbol     |\\({V\_{\text{W}}}\\)                                                                                                                                                              |
|Units      |\\({\text{m}^{3}}\\)                                                                                                                                                               |
|Equation   |\\[{V\_{\text{W}}}={V\_{\text{tank}}}\\]                                                                                                                                           |
|Description|<ul><li>\\({V\_{\text{W}}}\\) is the volume of water (\\({\text{m}^{3}}\\))</li><li>\\({V\_{\text{tank}}}\\) is the volume of the cylindrical tank (\\({\text{m}^{3}}\\))</li></ul>|
|Notes      |<ul><li>Based on [A:Volume-Coil-Negligible](./SecAssumps.md#assumpVCN). \\({V\_{\text{tank}}}\\) is defined in [DD:tankVolume](./SecDDs.md#DD:tankVolume).</li></ul>               |
|Source     |--                                                                                                                                                                                 |
|RefBy      |[FR:Find-Mass](./SecFRs.md#findMass)                                                                                                                                               |

<div align="center">

## Volume of the cylindrical tank {#DD:tankVolume}

</div>

|Refname    |DD:tankVolume                                                                                                                                                                                                                                                                                                  |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Volume of the cylindrical tank                                                                                                                                                                                                                                                                                 |
|Symbol     |\\({V\_{\text{tank}}}\\)                                                                                                                                                                                                                                                                                       |
|Units      |\\({\text{m}^{3}}\\)                                                                                                                                                                                                                                                                                           |
|Equation   |\\[{V\_{\text{tank}}}=π \left(\frac{D}{2}\right)^{2} L\\]                                                                                                                                                                                                                                                      |
|Description|<ul><li>\\({V\_{\text{tank}}}\\) is the volume of the cylindrical tank (\\({\text{m}^{3}}\\))</li><li>\\(π\\) is the ratio of circumference to diameter for any circle (Unitless)</li><li>\\(D\\) is the diameter of tank (\\({\text{m}}\\))</li><li>\\(L\\) is the length of tank (\\({\text{m}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                             |
|RefBy      |[DD:waterVolume_nopcm](./SecDDs.md#DD:waterVolume.nopcm) and [FR:Find-Mass](./SecFRs.md#findMass)                                                                                                                                                                                                              |

<div align="center">

## ODE parameter for water related to decay time {#DD:balanceDecayRate}

</div>

|Refname    |DD:balanceDecayRate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |ODE parameter for water related to decay time                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Symbol     |\\({τ\_{\text{W}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Equation   |\\[{τ\_{\text{W}}}=\frac{{m\_{\text{W}}} {C\_{\text{W}}}}{{h\_{\text{C}}} {A\_{\text{C}}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Description|<ul><li>\\({τ\_{\text{W}}}\\) is the ODE parameter for water related to decay time (\\({\text{s}}\\))</li><li>\\({m\_{\text{W}}}\\) is the mass of water (\\({\text{kg}}\\))</li><li>\\({C\_{\text{W}}}\\) is the specific heat capacity of water (\\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\))</li><li>\\({h\_{\text{C}}}\\) is the convective heat transfer coefficient between coil and water (\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\))</li><li>\\({A\_{\text{C}}}\\) is the heating coil surface area (\\({\text{m}^{2}}\\))</li></ul>|
|Source     |[koothoor2013](./SecReferences.md#koothoor2013)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|RefBy      |[IM:eBalanceOnWtr](./SecIMs.md#IM:eBalanceOnWtr) and [FR:Output-Input-Derived-Values](./SecFRs.md#outputInputDerivVals)                                                                                                                                                                                                                                                                                                                                                                                                                                        |
