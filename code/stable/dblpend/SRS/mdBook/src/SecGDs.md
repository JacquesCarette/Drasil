# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## The vector of velocity of the first object {#GD:velocityVector1}

</div>

|Refname    |GD:velocityVector1                                                                                                                                                                                                                                                                                                                                                           |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The vector of velocity of the first object                                                                                                                                                                                                                                                                                                                                   |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                              |
|Equation   |\\[{\boldsymbol{v}\_{1}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                           |
|Description|<ul><li>\\({\boldsymbol{v}\_{1}}\\) is the velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                             |

#### Detailed derivation of the vector of velocity: {#GD:velocityVector1Deriv}

At a given point in time, velocity is the time derivative of the position vector

\\[{\boldsymbol{v}\_{1}}=\frac{\\,d{\boldsymbol{p}\_{1}}}{\\,dt}\\]

The position vector is defined as \\({\boldsymbol{p}\_{1}}\\) (shown in [Fig:dblpend](./SecPhysSyst.md#Figure:dblpend).)

\\[{\boldsymbol{p}\_{1}}={L\_{1}}\\,\begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\\]

Applying this,

\\[\frac{\\,d{\boldsymbol{p}\_{1}}}{\\,dt}={L\_{1}}\\,\begin{bmatrix}\cos\left({θ\_{1}}\right)\\,{w\_{1}}\\\\\sin\left({θ\_{1}}\right)\\,{w\_{1}}\end{bmatrix}\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{L\_{1}}\\,\begin{bmatrix}\cos\left({θ\_{1}}\right)\\,{w\_{1}}\\\\\sin\left({θ\_{1}}\right)\\,{w\_{1}}\end{bmatrix}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

Therefore, using the chain rule,

\\[{\boldsymbol{v}\_{1}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

<div align="center">

## Velocity vector of the second object {#GD:velocityVector2}

</div>

|Refname    |GD:velocityVector2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity vector of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Equation   |\\[{\boldsymbol{v}\_{2}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                                                                                                                  |
|Description|<ul><li>\\({\boldsymbol{v}\_{2}}\\) is the velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |

#### Detailed derivation of velocity of the second object: {#GD:velocityVector2Deriv}

The velocity vector for the second object combines the velocity from the first pendulum with its own rotational motion.

\\[{\boldsymbol{v}\_{2}}=\frac{\\,d{\boldsymbol{p}\_{2}}}{\\,dt}\\]

The total velocity is expressed as the vector sum of these two velocity contributions.

\\[{\boldsymbol{p}\_{2}}={\boldsymbol{p}\_{1}}+{L\_{2}}\\,\begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[\frac{\\,d{\boldsymbol{p}\_{2}}}{\\,dt}=\left(\frac{\\,d{\boldsymbol{p}\_{1}}}{\\,dt}+{L\_{2}}\right)\\,\begin{bmatrix}\cos\left({θ\_{2}}\right)\\,{w\_{2}}\\\\\sin\left({θ\_{2}}\right)\\,{w\_{2}}\end{bmatrix}\\]

\\[\frac{\\,d{\boldsymbol{p}\_{1}}}{\\,dt}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[{\boldsymbol{v}\_{2}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

<div align="center">

## Acceleration vector of the first object {#GD:accelerationVector1}

</div>

|Refname    |GD:accelerationVector1                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration vector of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{\boldsymbol{a}\_{1}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({\boldsymbol{a}\_{1}}\\) is the acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                        |

#### Detailed derivation of acceleration of the first object: {#GD:accelerationVector1Deriv}

The acceleration vector combines centripetal and tangential acceleration components.

\\[{\boldsymbol{a}\_{1}}=\frac{\\,d{\boldsymbol{v}\_{1}}}{\\,dt}\\]

The centripetal acceleration points radially inward, while tangential acceleration is perpendicular to the rod.

\\[{\boldsymbol{v}\_{1}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

Both components are expressed as vectors and added using vector addition.

\\[\frac{\\,d{\boldsymbol{v}\_{1}}}{\\,dt}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

Centripetal component: -ω₁²L₁ × direction_vector (radial inward)

\\[{\boldsymbol{a}\_{1}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

Tangential component: α₁L₁ × perpendicular_direction_vector (tangential)

The vector sum preserves the underlying geometric relationships.

<div align="center">

## Acceleration vector of the second object {#GD:accelerationVector2}

</div>

|Refname    |GD:accelerationVector2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration vector of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Equation   |\\[{\boldsymbol{a}\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                                                                                                                               |
|Description|<ul><li>\\({\boldsymbol{a}\_{2}}\\) is the acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

#### Detailed derivation of acceleration of the second object: {#GD:accelerationVector2Deriv}

The second object's acceleration is the vector sum of the first object's acceleration and its own relative acceleration.

\\[{\boldsymbol{a}\_{2}}=\frac{\\,d{\boldsymbol{v}\_{2}}}{\\,dt}\\]

This captures the coupling between the two pendulums through their mechanical connection.

\\[{\boldsymbol{v}\_{2}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[\frac{\\,d{\boldsymbol{v}\_{2}}}{\\,dt}=\frac{\\,d{\boldsymbol{v}\_{1}}}{\\,dt}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[{\boldsymbol{a}\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[{\boldsymbol{a}\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

<div align="center">

## Force vector on the first object {#GD:forceVector1}

</div>

|Refname    |GD:forceVector1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force vector on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Equation   |\\[{\boldsymbol{F}\_{1}}=-\left({\boldsymbol{T}\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\right)+{\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+\begin{bmatrix}0\\\\-{m\_{1}}\\,g\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\({\boldsymbol{F}\_{1}}\\) is the force of the first object (\\({\text{N}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the the magnitude of the tension in the first rod (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the the magnitude of the tension in the second rod (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of force on the first object: {#GD:forceVector1Deriv}

The force vector combines tension forces and gravitational force as vectors.

\\[{\boldsymbol{F}\_{1}}={\boldsymbol{T}\_{1}}+{\boldsymbol{T}\_{2}}+{m\_{1}} \boldsymbol{g}\\]

The net force is obtained using vector addition, consistent with Newton's second law.

\\[{\boldsymbol{F}\_{1}}={m\_{1}} {\boldsymbol{a}\_{1}}\\]

Gravitational force acts vertically downward and is represented as a vector.

\\[{\boldsymbol{a}\_{1}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

\\[{\boldsymbol{F}\_{1}}=-\left({\boldsymbol{T}\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\right)+{\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+\begin{bmatrix}0\\\\-{m\_{1}}\\,g\end{bmatrix}\\]

\\[{\boldsymbol{F}\_{1}}=-\left({\boldsymbol{T}\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\right)+{\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+\begin{bmatrix}0\\\\-{m\_{1}}\\,g\end{bmatrix}\\]

<div align="center">

## Force vector on the second object {#GD:forceVector2}

</div>

|Refname    |GD:forceVector2                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force vector on the second object                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Equation   |\\[{\boldsymbol{F}\_{2}}=-\left({\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\right)+\begin{bmatrix}0\\\\-{m\_{2}}\\,g\end{bmatrix}\\]                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\({\boldsymbol{F}\_{2}}\\) is the force of the second object (\\({\text{N}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the the magnitude of the tension in the second rod (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                               |

#### Detailed derivation of force on the second object: {#GD:forceVector2Deriv}

The force on the second object combines tension from the second rod with gravitational effects.

\\[{\boldsymbol{F}\_{2}}={\boldsymbol{T}\_{2}}+{m\_{2}} \boldsymbol{g}\\]

The vector representation maintains consistency between force and acceleration.

\\[{\boldsymbol{F}\_{2}}={m\_{2}} {\boldsymbol{a}\_{2}}\\]

This approach naturally handles the coupling forces between the connected pendulum objects.

\\[{\boldsymbol{a}\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

\\[{\boldsymbol{F}\_{2}}=-\left({\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\right)+\begin{bmatrix}0\\\\-{m\_{2}}\\,g\end{bmatrix}\\]

\\[{\boldsymbol{F}\_{2}}=-\left({\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\right)+\begin{bmatrix}0\\\\-{m\_{2}}\\,g\end{bmatrix}\\]
