# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Speed {#DD:vecMag}

</div>

|Refname    |DD:vecMag                                                                                                                                                                               |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Speed                                                                                                                                                                                   |
|Symbol     |\\(v\\)                                                                                                                                                                                 |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                         |
|Equation   |\\[v=\|\boldsymbol{v}\text{(}t\text{)}\|\\]                                                                                                                                             |
|Description|<ul><li>\\(v\\) is the speed (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>                |
|Notes      |<ul><li>For a given velocity vector \\(\boldsymbol{v}\text{(}t\text{)}\\), the magnitude of the vector (\\(\|\boldsymbol{v}\text{(}t\text{)}\|\\)) is the scalar called speed.</li></ul>|
|Source     |--                                                                                                                                                                                      |
|RefBy      |[DD:speedIY](./SecDDs.md#DD:speedIY) and [DD:speedIX](./SecDDs.md#DD:speedIX)                                                                                                           |

<div align="center">

## \\(x\\)-component of initial velocity {#DD:speedIX}

</div>

|Refname    |DD:speedIX                                                                                                                                                                                                                                                                |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |\\(x\\)-component of initial velocity                                                                                                                                                                                                                                     |
|Symbol     |\\(v^{\text{i}}\_{\text{x}}\\)                                                                                                                                                                                                                                            |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                           |
|Equation   |\\[v^{\text{i}}\_{\text{x}}=v^{\text{i}}\\,\cos\left(θ\right)\\]                                                                                                                                                                                                          |
|Description|<ul><li>\\(v^{\text{i}}\_{\text{x}}\\) is the \\(x\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(v^{\text{i}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(θ\\) is the launch angle (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\(v^{\text{i}}\\) is from [DD:vecMag](./SecDDs.md#DD:vecMag).</li><li>\\(θ\\) is shown in [Fig:Launch](./SecPhysSyst.md#Figure:Launch).</li></ul>                                                                                                                |
|Source     |--                                                                                                                                                                                                                                                                        |
|RefBy      |[IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist)                                                                                                                                                                                                                    |

<div align="center">

## \\(y\\)-component of initial velocity {#DD:speedIY}

</div>

|Refname    |DD:speedIY                                                                                                                                                                                                                                                                |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |\\(y\\)-component of initial velocity                                                                                                                                                                                                                                     |
|Symbol     |\\(v^{\text{i}}\_{\text{y}}\\)                                                                                                                                                                                                                                            |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                           |
|Equation   |\\[v^{\text{i}}\_{\text{y}}=v^{\text{i}}\\,\sin\left(θ\right)\\]                                                                                                                                                                                                          |
|Description|<ul><li>\\(v^{\text{i}}\_{\text{y}}\\) is the \\(y\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(v^{\text{i}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(θ\\) is the launch angle (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\(v^{\text{i}}\\) is from [DD:vecMag](./SecDDs.md#DD:vecMag).</li><li>\\(θ\\) is shown in [Fig:Launch](./SecPhysSyst.md#Figure:Launch).</li></ul>                                                                                                                |
|Source     |--                                                                                                                                                                                                                                                                        |
|RefBy      |[IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime)                                                                                                                                                                                                                    |
