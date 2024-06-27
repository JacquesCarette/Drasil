# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Effective heat transfer coefficient between clad and fuel surface {#DD:htTransCladFuel}

</div>

|Refname    |DD:htTransCladFuel                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Effective heat transfer coefficient between clad and fuel surface                                                                                                                                                                                                                                                                                                                     |
|Symbol     |\\({h\_{\text{g}}}\\)                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{h\_{\text{g}}}=\frac{2 {k\_{\text{c}}} {h\_{\text{p}}}}{2 {k\_{\text{c}}}+{τ\_{\text{c}}} {h\_{\text{p}}}}\\]                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({h\_{\text{g}}}\\) is the effective heat transfer coefficient between clad and fuel surface (\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\))</li><li>\\({k\_{\text{c}}}\\) is the clad conductivity (Unitless)</li><li>\\({h\_{\text{p}}}\\) is the initial gap film conductance (Unitless)</li><li>\\({τ\_{\text{c}}}\\) is the clad thickness (Unitless)</li></ul>|

<div align="center">

## Convective heat transfer coefficient between clad and coolant {#DD:htTransCladCool}

</div>

|Refname    |DD:htTransCladCool                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Convective heat transfer coefficient between clad and coolant                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\({h\_{\text{c}}}\\)                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{h\_{\text{c}}}=\frac{2 {k\_{\text{c}}} {h\_{\text{b}}}}{2 {k\_{\text{c}}}+{τ\_{\text{c}}} {h\_{\text{b}}}}\\]                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({h\_{\text{c}}}\\) is the convective heat transfer coefficient between clad and coolant (\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\))</li><li>\\({k\_{\text{c}}}\\) is the clad conductivity (Unitless)</li><li>\\({h\_{\text{b}}}\\) is the initial coolant film conductance (Unitless)</li><li>\\({τ\_{\text{c}}}\\) is the clad thickness (Unitless)</li></ul>|

