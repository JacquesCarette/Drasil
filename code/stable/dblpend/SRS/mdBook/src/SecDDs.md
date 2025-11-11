# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Velocity {#DD:positionGDD}

</div>

|Refname    |DD:positionGDD                                                                                                                                                                                                                           |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity                                                                                                                                                                                                                                 |
|Symbol     |\\(\boldsymbol{v}\text{(}t\text{)}\\)                                                                                                                                                                                                    |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                          |
|Equation   |\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                  |
|Description|<ul><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                       |
|RefBy      |[GD:velocityY2](./SecGDs.md#GD:velocityY2), [GD:velocityY1](./SecGDs.md#GD:velocityY1), [GD:velocityX2](./SecGDs.md#GD:velocityX2), and [GD:velocityX1](./SecGDs.md#GD:velocityX1)                                                       |

<div align="center">

## Horizontal position of the first object {#DD:positionXDD1}

</div>

|Refname    |DD:positionXDD1                                                                                                                                                                                                                                              |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal position of the first object                                                                                                                                                                                                                      |
|Symbol     |\\({p\_{\text{x}1}}\\)                                                                                                                                                                                                                                       |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                             |
|Equation   |\\[{p\_{\text{x}1}}={L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                  |
|Description|<ul><li>\\({p\_{\text{x}1}}\\) is the horizontal position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{x}1}}\\) is the horizontal position</li><li>\\({p\_{\text{x}1}}\\) is shown in [Fig:dblpend](./SecPhysSyst.md#Figure:dblpend).</li></ul>                                                                                               |
|Source     |--                                                                                                                                                                                                                                                           |
|RefBy      |[GD:velocityX1](./SecGDs.md#GD:velocityX1)                                                                                                                                                                                                                   |

<div align="center">

## Vertical position of the first object {#DD:positionYDD1}

</div>

|Refname    |DD:positionYDD1                                                                                                                                                                                                                                            |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical position of the first object                                                                                                                                                                                                                      |
|Symbol     |\\({p\_{\text{y}1}}\\)                                                                                                                                                                                                                                     |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                           |
|Equation   |\\[{p\_{\text{y}1}}=-{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                               |
|Description|<ul><li>\\({p\_{\text{y}1}}\\) is the vertical position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{y}1}}\\) is the vertical position</li><li>\\({p\_{\text{y}1}}\\) is shown in [Fig:dblpend](./SecPhysSyst.md#Figure:dblpend).</li></ul>                                                                                               |
|Source     |--                                                                                                                                                                                                                                                         |
|RefBy      |[GD:velocityY1](./SecGDs.md#GD:velocityY1)                                                                                                                                                                                                                 |

<div align="center">

## Horizontal position of the second object {#DD:positionXDD2}

</div>

|Refname    |DD:positionXDD2                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal position of the second object                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\({p\_{\text{x}2}}\\)                                                                                                                                                                                                                                                                                                                                           |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{p\_{\text{x}2}}={p\_{\text{x}1}}+{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({p\_{\text{x}2}}\\) is the horizontal position of the second object (\\({\text{m}}\\))</li><li>\\({p\_{\text{x}1}}\\) is the horizontal position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{x}2}}\\) is the horizontal position</li><li>\\({p\_{\text{x}2}}\\) is shown in [Fig:dblpend](./SecPhysSyst.md#Figure:dblpend).</li></ul>                                                                                                                                                                                                   |
|Source     |--                                                                                                                                                                                                                                                                                                                                                               |
|RefBy      |[GD:velocityX2](./SecGDs.md#GD:velocityX2)                                                                                                                                                                                                                                                                                                                       |

<div align="center">

## Vertical position of the second object {#DD:positionYDD2}

</div>

|Refname    |DD:positionYDD2                                                                                                                                                                                                                                                                                                                                              |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical position of the second object                                                                                                                                                                                                                                                                                                                       |
|Symbol     |\\({p\_{\text{y}2}}\\)                                                                                                                                                                                                                                                                                                                                       |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[{p\_{\text{y}2}}={p\_{\text{y}1}}-{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\({p\_{\text{y}2}}\\) is the vertical position of the second object (\\({\text{m}}\\))</li><li>\\({p\_{\text{y}1}}\\) is the vertical position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{y}2}}\\) is the vertical position</li><li>\\({p\_{\text{y}2}}\\) is shown in [Fig:dblpend](./SecPhysSyst.md#Figure:dblpend).</li></ul>                                                                                                                                                                                                 |
|Source     |--                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |[GD:velocityY2](./SecGDs.md#GD:velocityY2)                                                                                                                                                                                                                                                                                                                   |

<div align="center">

## Acceleration {#DD:accelerationGDD}

</div>

|Refname    |DD:accelerationGDD                                                                                                                                                                                                                                              |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration                                                                                                                                                                                                                                                    |
|Symbol     |\\(\boldsymbol{a}\text{(}t\text{)}\\)                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                             |
|Equation   |\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                              |
|RefBy      |                                                                                                                                                                                                                                                                |

<div align="center">

## Force vector {#DD:forceGDD}

</div>

|Refname    |DD:forceGDD                                                                                                                                                                                                                           |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force vector                                                                                                                                                                                                                          |
|Symbol     |\\(\boldsymbol{F'}\\)                                                                                                                                                                                                                 |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                      |
|Equation   |\\[\boldsymbol{F'}=m \boldsymbol{a}\text{(}t\text{)}\\]                                                                                                                                                                               |
|Description|<ul><li>\\(\boldsymbol{F'}\\) is the force vector (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                      |
