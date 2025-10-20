# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Velocity of the first object {#DD:velocityVecDD1}

</div>

|Refname    |DD:velocityVecDD1                                                                                                                                                                                                                                            |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity of the first object                                                                                                                                                                                                                                 |
|Symbol     |\\({\boldsymbol{v}\_{1}}\\)                                                                                                                                                                                                                                  |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                              |
|Equation   |\\[{\boldsymbol{v}\_{1}}=\frac{\\,d{\boldsymbol{p}\_{1}}}{\\,dt}\\]                                                                                                                                                                                          |
|Description|<ul><li>\\({\boldsymbol{v}\_{1}}\\) is the velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({\boldsymbol{p}\_{1}}\\) is the position of the first object (\\({\text{m}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                             |

<div align="center">

## Velocity of the second object {#DD:velocityVecDD2}

</div>

|Refname    |DD:velocityVecDD2                                                                                                                                                                                                                                              |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity of the second object                                                                                                                                                                                                                                  |
|Symbol     |\\({\boldsymbol{v}\_{2}}\\)                                                                                                                                                                                                                                    |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                |
|Equation   |\\[{\boldsymbol{v}\_{2}}=\frac{\\,d{\boldsymbol{p}\_{2}}}{\\,dt}\\]                                                                                                                                                                                            |
|Description|<ul><li>\\({\boldsymbol{v}\_{2}}\\) is the velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({\boldsymbol{p}\_{2}}\\) is the position of the second object (\\({\text{m}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                             |
|RefBy      |                                                                                                                                                                                                                                                               |

<div align="center">

## Acceleration of the first object {#DD:accelVecDD1}

</div>

|Refname    |DD:accelVecDD1                                                                                                                                                                                                                                                                      |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration of the first object                                                                                                                                                                                                                                                    |
|Symbol     |\\({\boldsymbol{a}\_{1}}\\)                                                                                                                                                                                                                                                         |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                 |
|Equation   |\\[{\boldsymbol{a}\_{1}}=\frac{\\,d{\boldsymbol{v}\_{1}}}{\\,dt}\\]                                                                                                                                                                                                                 |
|Description|<ul><li>\\({\boldsymbol{a}\_{1}}\\) is the acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({\boldsymbol{v}\_{1}}\\) is the velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                  |
|RefBy      |                                                                                                                                                                                                                                                                                    |

<div align="center">

## Acceleration of the second object {#DD:accelVecDD2}

</div>

|Refname    |DD:accelVecDD2                                                                                                                                                                                                                                                                        |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration of the second object                                                                                                                                                                                                                                                     |
|Symbol     |\\({\boldsymbol{a}\_{2}}\\)                                                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                   |
|Equation   |\\[{\boldsymbol{a}\_{2}}=\frac{\\,d{\boldsymbol{v}\_{2}}}{\\,dt}\\]                                                                                                                                                                                                                   |
|Description|<ul><li>\\({\boldsymbol{a}\_{2}}\\) is the acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({\boldsymbol{v}\_{2}}\\) is the velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                                                                      |

<div align="center">

## Force of the first object {#DD:forceVecDD1}

</div>

|Refname    |DD:forceVecDD1                                                                                                                                                                                                                                                     |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force of the first object                                                                                                                                                                                                                                          |
|Symbol     |\\({\boldsymbol{F}\_{1}}\\)                                                                                                                                                                                                                                        |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                   |
|Equation   |\\[{\boldsymbol{F}\_{1}}=m\\,{\boldsymbol{a}\_{1}}\\]                                                                                                                                                                                                              |
|Description|<ul><li>\\({\boldsymbol{F}\_{1}}\\) is the force of the first object (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\({\boldsymbol{a}\_{1}}\\) is the acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                   |

<div align="center">

## Force of the second object {#DD:forceVecDD2}

</div>

|Refname    |DD:forceVecDD2                                                                                                                                                                                                                                                       |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force of the second object                                                                                                                                                                                                                                           |
|Symbol     |\\({\boldsymbol{F}\_{2}}\\)                                                                                                                                                                                                                                          |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                     |
|Equation   |\\[{\boldsymbol{F}\_{2}}=m\\,{\boldsymbol{a}\_{2}}\\]                                                                                                                                                                                                                |
|Description|<ul><li>\\({\boldsymbol{F}\_{2}}\\) is the force of the second object (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\({\boldsymbol{a}\_{2}}\\) is the acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                   |
|RefBy      |                                                                                                                                                                                                                                                                     |
