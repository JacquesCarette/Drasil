# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that GlassBR is based on.

<div align="center">

## Safety Probability {#TM:isSafeProb}

</div>

|Refname    |TM:isSafeProb                                                                                                                                                                                                                                                            |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Safety Probability                                                                                                                                                                                                                                                       |
|Equation   |\\[\mathit{isSafeProb}={P\_{\text{f}}}\lt{}{P\_{\text{f}\text{tol}}}\\]                                                                                                                                                                                                  |
|Description|<ul><li>\\(\mathit{isSafeProb}\\) is the probability of failure safety requirement (Unitless)</li><li>\\({P\_{\text{f}}}\\) is the probability of failure (Unitless)</li><li>\\({P\_{\text{f}\text{tol}}}\\) is the tolerable probability of failure (Unitless)</li></ul>|
|Notes      |<ul><li>If \\(\mathit{isSafeProb}\\), the structure is considered safe.</li></ul>                                                                                                                                                                                        |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                                                                  |
|RefBy      |                                                                                                                                                                                                                                                                         |

<div align="center">

## Safety Load {#TM:isSafeLoad}

</div>

|Refname    |TM:isSafeLoad                                                                                                                                                                                                                                                                   |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Safety Load                                                                                                                                                                                                                                                                     |
|Equation   |\\[\mathit{isSafeLoad}=\mathit{capacity}\gt{}\mathit{Load}\\]                                                                                                                                                                                                                   |
|Description|<ul><li>\\(\mathit{isSafeLoad}\\) is the load resistance safety requirement (Unitless)</li><li>\\(\mathit{capacity}\\) is the capacity or load resistance (\\({\text{Pa}}\\))</li><li>\\(\mathit{Load}\\) is the applied load (demand) or pressure (\\({\text{Pa}}\\))</li></ul>|
|Notes      |<ul><li>If \\(\mathit{isSafeLoad}\\), the structure is considered safe.</li></ul>                                                                                                                                                                                               |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                                                                         |
|RefBy      |                                                                                                                                                                                                                                                                                |

