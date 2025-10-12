# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## Velocity vector of the first object {#GD:velocityVector1}

</div>

|Refname    |GD:velocityVector1                                                                                                                                                                                                                                                                                                                                              |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity vector of the first object                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{v\_{1}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                           |
|Description|<ul><li>\\({v\_{1}}\\) is the velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                              |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                |

#### Detailed derivation of velocity of the first object: {#GD:velocityVector1Deriv}

The velocity vector in Clifford algebra represents both magnitude and direction as a unified geometric entity.

\\[{v\_{1}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

Using the 2D Clifford space Cl(2,0), the velocity is expressed as a vector with components in the e₁ and e₂ basis directions.

where the velocity vector combines angular velocity and rod length in Clifford space

This approach provides a natural geometric representation that preserves rotational relationships.

Direction vector = cos(θ₁)e₁ + sin(θ₁)e₂ using basis vectors e₁, e₂

The angular velocity ω₁ and rod length L₁ combine with the direction vector to form the complete velocity vector.

The geometric product preserves both magnitude and geometric orientation.

Unlike traditional component-wise vector addition, this geometric product maintains the intrinsic geometric structure.

<div align="center">

## Velocity vector of the second object {#GD:velocityVector2}

</div>

|Refname    |GD:velocityVector2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity vector of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Equation   |\\[{v\_{2}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                                                                                                                  |
|Description|<ul><li>\\({v\_{2}}\\) is the velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |

#### Detailed derivation of velocity of the second object: {#GD:velocityVector2Deriv}

The velocity vector for the second object combines the velocity from the first pendulum with its own rotational motion.

\\[{v\_{2}}={w\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+{w\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

In Clifford algebra, this vector addition preserves the geometric relationships between the two connected pendulums.

This represents the geometric sum of the first pendulum's velocity and the second pendulum's relative velocity

The total velocity is expressed as the sum of two vectors in the same geometric space.

<div align="center">

## Acceleration vector of the first object {#GD:accelerationVector1}

</div>

|Refname    |GD:accelerationVector1                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration vector of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{a\_{1}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({a\_{1}}\\) is the acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                           |

#### Detailed derivation of acceleration of the first object: {#GD:accelerationVector1Deriv}

The acceleration vector in Clifford algebra combines centripetal and tangential acceleration components.

\\[{a\_{1}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}\\]

The centripetal acceleration points radially inward, while tangential acceleration is perpendicular to the rod.

where the acceleration combines centripetal and tangential components in Clifford space

Both components are naturally expressed as vector and added using Clifford geometric operations.

= centripetal_vector + tangential_vector

Centripetal component: -ω₁²L₁ × direction_vector (radial inward)

Both terms maintain their geometric significance through the Clifford algebra representation

Tangential component: α₁L₁ × perpendicular_direction_vector (tangential)

The geometric sum preserves the underlying geometric relationships without component-wise decomposition.

<div align="center">

## Acceleration vector of the second object {#GD:accelerationVector2}

</div>

|Refname    |GD:accelerationVector2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration vector of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Equation   |\\[{a\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                                                                                                                               |
|Description|<ul><li>\\({a\_{2}}\\) is the acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |

#### Detailed derivation of acceleration of the second object: {#GD:accelerationVector2Deriv}

The second object's acceleration is the geometric sum of the first object's acceleration and its own relative acceleration.

\\[{a\_{2}}=-{w\_{1}}^{2}\\,{L\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}+{α\_{1}}\\,{L\_{1}} \begin{bmatrix}\cos\left({θ\_{1}}\right)\\\\\sin\left({θ\_{1}}\right)\end{bmatrix}+-{w\_{2}}^{2}\\,{L\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+{α\_{2}}\\,{L\_{2}} \begin{bmatrix}\cos\left({θ\_{2}}\right)\\\\\sin\left({θ\_{2}}\right)\end{bmatrix}\\]

This captures the coupling between the two pendulums through their mechanical connection.

This represents the total acceleration as a vector sum in Clifford geometric space

The Clifford algebra representation preserves the geometric relationships in this coupled system.

<div align="center">

## Force vector on the first object {#GD:forceVector1}

</div>

|Refname    |GD:forceVector1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force vector on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[{f\_{1}}=-\left({\boldsymbol{T}\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\right)+{\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+\begin{bmatrix}0\\\\-{m\_{1}}\\,g\end{bmatrix}\\]                                                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\({f\_{1}}\\) is the force of the first object (\\({\text{N}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the tension of the first rod (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second rod (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of force on the first object: {#GD:forceVector1Deriv}

The force vector in Clifford algebra combines inertial and gravitational forces as vectors.

\\[{f\_{1}}=-\left({\boldsymbol{T}\_{1}} \begin{bmatrix}\sin\left({θ\_{1}}\right)\\\\-\cos\left({θ\_{1}}\right)\end{bmatrix}\right)+{\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}+\begin{bmatrix}0\\\\-{m\_{1}}\\,g\end{bmatrix}\\]

The inertial force is proportional to the force vector, preserving Newton's second law in geometric form.

where force equals mass times acceleration plus gravitational force in vector form

Gravitational force acts vertically downward and is naturally represented as a vector.

<div align="center">

## Force vector on the second object {#GD:forceVector2}

</div>

|Refname    |GD:forceVector2                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force vector on the second object                                                                                                                                                                                                                                                                                                                                                                                                        |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Equation   |\\[{f\_{2}}=-\left({\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\right)+\begin{bmatrix}0\\\\-{m\_{2}}\\,g\end{bmatrix}\\]                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\({f\_{2}}\\) is the force of the second object (\\({\text{N}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second rod (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|RefBy      |[IM:calOfAngle2](./SecIMs.md#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                             |

#### Detailed derivation of force on the second object: {#GD:forceVector2Deriv}

The force on the second object combines its inertial response with gravitational effects.

\\[{f\_{2}}=-\left({\boldsymbol{T}\_{2}} \begin{bmatrix}\sin\left({θ\_{2}}\right)\\\\-\cos\left({θ\_{2}}\right)\end{bmatrix}\right)+\begin{bmatrix}0\\\\-{m\_{2}}\\,g\end{bmatrix}\\]

The Clifford algebra representation maintains the geometric consistency between force and acceleration.

This represents the total force as a vector sum in Clifford geometric space

This approach naturally handles the coupling forces between the connected pendulum objects.

= mass × acceleration_multivector + gravitational_force_multivector

Key GA operations demonstrated: geometric product (combines magnitude and direction),

Demonstrating how Newton's second law extends naturally to geometric algebra

vector addition in Clifford space (preserves geometric relationships),

scalar multiplication with multivectors (maintains geometric structure).
