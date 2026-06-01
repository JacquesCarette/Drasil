# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## The \\(x\\)-component of velocity of the first object {#GD:velocityX1}

</div>

|Refname    |GD:velocityX1                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of velocity of the first object                                                                                                                                                                                                                                                                                                                              |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{v\_{\text{x}1}}={w\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                             |
|Description|<ul><li>\\({v\_{\text{x}1}}\\) is the horizontal velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of the \\(x\\)-component of velocity: {#GD:velocityX1Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](./SecDDs.md#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the horizontal position that is defined in [DD:positionXDD1](./SecDDs.md#DD:positionXDD1)

\\[{p\_{\text{x}1}}={L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]

Applying this,

\\[{v\_{\text{x}1}}=\frac{\\,d{L\_{1}}\\,\sin\left({θ\_{1}}\right)}{\\,dt}\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{x}1}}={L\_{1}}\\,\frac{\\,d\sin\left({θ\_{1}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{x}1}}={w\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(y\\)-component of velocity of the first object {#GD:velocityY1}

</div>

|Refname    |GD:velocityY1                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of velocity of the first object                                                                                                                                                                                                                                                                                                                            |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                  |
|Equation   |\\[{v\_{\text{y}1}}={w\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                           |
|Description|<ul><li>\\({v\_{\text{y}1}}\\) is the vertical velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                               |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                 |

#### Detailed derivation of the \\(y\\)-component of velocity: {#GD:velocityY1Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](./SecDDs.md#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the vertical position that is defined in [DD:positionYDD1](./SecDDs.md#DD:positionYDD1)

\\[{p\_{\text{y}1}}=-{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]

Applying this,

\\[{v\_{\text{y}1}}=-\left(\frac{\\,d{L\_{1}}\\,\cos\left({θ\_{1}}\right)}{\\,dt}\right)\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{y}1}}=-{L\_{1}}\\,\frac{\\,d\cos\left({θ\_{1}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{y}1}}={w\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(x\\)-component of velocity of the second object {#GD:velocityX2}

</div>

|Refname    |GD:velocityX2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of velocity of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\({v\_{\text{x}2}}\\) is the horizontal velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({v\_{\text{x}1}}\\) is the horizontal velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of the \\(x\\)-component of velocity: {#GD:velocityX2Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](./SecDDs.md#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the horizontal position that is defined in [DD:positionXDD2](./SecDDs.md#DD:positionXDD2)

\\[{p\_{\text{x}2}}={p\_{\text{x}1}}+{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

Applying this,

\\[{v\_{\text{x}2}}=\frac{\\,d{p\_{\text{x}1}}+{L\_{2}}\\,\sin\left({θ\_{2}}\right)}{\\,dt}\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(y\\)-component of velocity of the second object {#GD:velocityY2}

</div>

|Refname    |GD:velocityY2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of velocity of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                            |
|Description|<ul><li>\\({v\_{\text{y}2}}\\) is the vertical velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({v\_{\text{y}1}}\\) is the vertical velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of the \\(y\\)-component of velocity: {#GD:velocityY2Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](./SecDDs.md#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the vertical position that is defined in [DD:positionYDD2](./SecDDs.md#DD:positionYDD2)

\\[{p\_{\text{y}2}}={p\_{\text{y}1}}-{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]

Applying this,

\\[{v\_{\text{y}2}}=-\left(\frac{\\,d{p\_{\text{y}1}}-{L\_{2}}\\,\cos\left({θ\_{2}}\right)}{\\,dt}\right)\\]

Therefore, using the chain rule,

\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(x\\)-component of acceleration of the first object {#GD:accelerationX1}

</div>

|Refname    |GD:accelerationX1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of acceleration of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Equation   |\\[{a\_{\text{x}1}}=-{w\_{1}}^{2}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)+{α\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                               |
|Description|<ul><li>\\({a\_{\text{x}1}}\\) is the horizontal acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                              |

#### Detailed derivation of the \\(x\\)-component of acceleration: {#GD:accelerationX1Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{x}1}}={w\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{x}1}}=\frac{\\,d{w\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{x}1}}=\frac{\\,d{w\_{1}}}{\\,dt}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)-{w\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\,\frac{\\,d{θ\_{1}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{x}1}}=-{w\_{1}}^{2}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)+{α\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(y\\)-component of acceleration of the first object {#GD:accelerationY1}

</div>

|Refname    |GD:accelerationY1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of acceleration of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[{a\_{\text{y}1}}={w\_{1}}^{2}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)+{α\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                              |
|Description|<ul><li>\\({a\_{\text{y}1}}\\) is the vertical acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of the \\(y\\)-component of acceleration: {#GD:accelerationY1Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the vertical velocity to be

\\[{v\_{\text{y}1}}={w\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{y}1}}=\frac{\\,d{w\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{y}1}}=\frac{\\,d{w\_{1}}}{\\,dt}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)+{w\_{1}}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)\\,\frac{\\,d{θ\_{1}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{y}1}}={w\_{1}}^{2}\\,{L\_{1}}\\,\cos\left({θ\_{1}}\right)+{α\_{1}}\\,{L\_{1}}\\,\sin\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(x\\)-component of acceleration of the second object {#GD:accelerationX2}

</div>

|Refname    |GD:accelerationX2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of acceleration of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{a\_{\text{x}2}}={a\_{\text{x}1}}-{w\_{2}}^{2}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)+{α\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Description|<ul><li>\\({a\_{\text{x}2}}\\) is the horizontal acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({a\_{\text{x}1}}\\) is the horizontal acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |

#### Detailed derivation of the \\(x\\)-component of acceleration: {#GD:accelerationX2Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{x}2}}=\frac{\\,d{v\_{\text{x}1}}+{w\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{x}2}}={a\_{\text{x}1}}-{w\_{2}}^{2}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)+{α\_{2}}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(y\\)-component of acceleration of the second object {#GD:accelerationY2}

</div>

|Refname    |GD:accelerationY2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of acceleration of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Equation   |\\[{a\_{\text{y}2}}={a\_{\text{y}1}}+{w\_{2}}^{2}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)+{α\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Description|<ul><li>\\({a\_{\text{y}2}}\\) is the vertical acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({a\_{\text{y}1}}\\) is the vertical acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of the \\(y\\)-component of acceleration: {#GD:accelerationY2Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{y}2}}=\frac{\\,d{v\_{\text{y}1}}+{w\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{y}2}}={a\_{\text{y}1}}+{w\_{2}}^{2}\\,{L\_{2}}\\,\cos\left({θ\_{2}}\right)+{α\_{2}}\\,{L\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

<div align="center">

## Horizontal force on the first object {#GD:xForce1}

</div>

|Refname    |GD:xForce1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal force on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{1}}\\,\sin\left({θ\_{1}}\right)+{\boldsymbol{T}\_{2}}\\,\sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                             |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the tension of the first object (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of force on the first object: {#GD:xForce1Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{1}}\\,\sin\left({θ\_{1}}\right)+{\boldsymbol{T}\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

<div align="center">

## Vertical force on the first object {#GD:yForce1}

</div>

|Refname    |GD:yForce1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical force on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{1}}\\,\cos\left({θ\_{1}}\right)-{\boldsymbol{T}\_{2}}\\,\cos\left({θ\_{2}}\right)-{m\_{1}}\\,\boldsymbol{g}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the tension of the first object (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of force on the first object: {#GD:yForce1Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{1}}\\,\cos\left({θ\_{1}}\right)-{\boldsymbol{T}\_{2}}\\,\cos\left({θ\_{2}}\right)-{m\_{1}}\\,\boldsymbol{g}\\]

<div align="center">

## Horizontal force on the second object {#GD:xForce2}

</div>

|Refname    |GD:xForce2                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal force on the second object                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{2}}\\,\sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                           |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                  |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                        |

#### Detailed derivation of force on the second object: {#GD:xForce2Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{2}}\\,\sin\left({θ\_{2}}\right)\\]

<div align="center">

## Vertical force on the second object {#GD:yForce2}

</div>

|Refname    |GD:yForce2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical force on the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{2}}\\,\cos\left({θ\_{2}}\right)-{m\_{2}}\\,\boldsymbol{g}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |

#### Detailed derivation of force on the second object: {#GD:yForce2Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{2}}\\,\cos\left({θ\_{2}}\right)-{m\_{2}}\\,\boldsymbol{g}\\]
