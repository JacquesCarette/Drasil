# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## \\(x\\)-component of initial position {#DD:positionIX}

</div>

|Refname    |DD:positionIX                                                                                                                                                                                                                                                             |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |\\(x\\)-component of initial position                                                                                                                                                                                                                                     |
|Symbol     |\\({{p\_{\text{x}}}^{\text{i}}}\\)                                                                                                                                                                                                                                        |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                          |
|Equation   |\\[{{p\_{\text{x}}}^{\text{i}}}={L\_{\text{rod}}}\\,\sin\left({θ\_{i}}\right)\\]                                                                                                                                                                                          |
|Description|<ul><li>\\({{p\_{\text{x}}}^{\text{i}}}\\) is the \\(x\\)-component of initial position (\\({\text{m}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{i}}\\) is the initial pendulum angle (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({{p\_{\text{x}}}^{\text{i}}}\\) is the horizontal position</li><li>\\({{p\_{\text{x}}}^{\text{i}}}\\) is shown in [Fig:sglpend](./SecPhysSyst.md#Figure:sglpend).</li></ul>                                                                                    |
|Source     |--                                                                                                                                                                                                                                                                        |
|RefBy      |                                                                                                                                                                                                                                                                          |

<div align="center">

## \\(y\\)-component of initial position {#DD:positionIY}

</div>

|Refname    |DD:positionIY                                                                                                                                                                                                                                                             |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |\\(y\\)-component of initial position                                                                                                                                                                                                                                     |
|Symbol     |\\({{p\_{\text{y}}}^{\text{i}}}\\)                                                                                                                                                                                                                                        |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                          |
|Equation   |\\[{{p\_{\text{y}}}^{\text{i}}}=-{L\_{\text{rod}}}\\,\cos\left({θ\_{i}}\right)\\]                                                                                                                                                                                         |
|Description|<ul><li>\\({{p\_{\text{y}}}^{\text{i}}}\\) is the \\(y\\)-component of initial position (\\({\text{m}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{i}}\\) is the initial pendulum angle (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({{p\_{\text{y}}}^{\text{i}}}\\) is the vertical position</li><li>\\({{p\_{\text{y}}}^{\text{i}}}\\) is shown in [Fig:sglpend](./SecPhysSyst.md#Figure:sglpend).</li></ul>                                                                                      |
|Source     |--                                                                                                                                                                                                                                                                        |
|RefBy      |                                                                                                                                                                                                                                                                          |

<div align="center">

## Frequency {#DD:frequencyDD}

</div>

|Refname    |DD:frequencyDD                                                                                                                                  |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Frequency                                                                                                                                       |
|Symbol     |\\(f\\)                                                                                                                                         |
|Units      |\\({\text{Hz}}\\)                                                                                                                               |
|Equation   |\\[f=\frac{1}{T}\\]                                                                                                                             |
|Description|<ul><li>\\(f\\) is the frequency (\\({\text{Hz}}\\))</li><li>\\(T\\) is the period (\\({\text{s}}\\))</li></ul>                                 |
|Notes      |<ul><li>\\(f\\) is the number of back and forth swings in one second</li></ul>                                                                  |
|Source     |--                                                                                                                                              |
|RefBy      |[GD:periodPend](./SecGDs.md#GD:periodPend), [DD:periodSHMDD](./SecDDs.md#DD:periodSHMDD), and [GD:angFrequencyGD](./SecGDs.md#GD:angFrequencyGD)|

<div align="center">

## Angular frequency {#DD:angFrequencyDD}

</div>

|Refname    |DD:angFrequencyDD                                                                                                                                                                                         |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular frequency                                                                                                                                                                                         |
|Symbol     |\\(Ω\\)                                                                                                                                                                                                   |
|Units      |\\({\text{s}}\\)                                                                                                                                                                                          |
|Equation   |\\[Ω=\frac{2\\,π}{T}\\]                                                                                                                                                                                   |
|Description|<ul><li>\\(Ω\\) is the angular frequency (\\({\text{s}}\\))</li><li>\\(π\\) is the ratio of circumference to diameter for any circle (Unitless)</li><li>\\(T\\) is the period (\\({\text{s}}\\))</li></ul>|
|Notes      |<ul><li>\\(T\\) is from [DD:periodSHMDD](./SecDDs.md#DD:periodSHMDD)</li></ul>                                                                                                                            |
|Source     |--                                                                                                                                                                                                        |
|RefBy      |[GD:periodPend](./SecGDs.md#GD:periodPend)                                                                                                                                                                |

<div align="center">

## Period {#DD:periodSHMDD}

</div>

|Refname    |DD:periodSHMDD                                                                                                 |
|:----------|:--------------------------------------------------------------------------------------------------------------|
|Label      |Period                                                                                                         |
|Symbol     |\\(T\\)                                                                                                        |
|Units      |\\({\text{s}}\\)                                                                                               |
|Equation   |\\[T=\frac{1}{f}\\]                                                                                            |
|Description|<ul><li>\\(T\\) is the period (\\({\text{s}}\\))</li><li>\\(f\\) is the frequency (\\({\text{Hz}}\\))</li></ul>|
|Notes      |<ul><li>\\(T\\) is from [DD:frequencyDD](./SecDDs.md#DD:frequencyDD)</li></ul>                                 |
|Source     |--                                                                                                             |
|RefBy      |[GD:periodPend](./SecGDs.md#GD:periodPend) and [DD:angFrequencyDD](./SecDDs.md#DD:angFrequencyDD)              |
