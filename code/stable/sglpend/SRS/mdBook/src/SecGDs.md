# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## The \\(x\\)-component of velocity of the pendulum {#GD:velocityIX}

</div>

|Refname    |GD:velocityIX                                                                                                                                                                                                                                                                                                                                               |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of velocity of the pendulum                                                                                                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[{v\_{\text{x}}}=ω {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]                                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\({v\_{\text{x}}}\\) is the \\(x\\)-component of velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                          |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of the \\(x\\)-component of velocity: {#GD:velocityIXDeriv}

At a given point in time, velocity may be defined as

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the horizontal position

\\[{p\_{\text{x}}}={L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]

Applying this,

\\[{v\_{\text{x}}}=\frac{\\,d{L\_{\text{rod}}} \sin\left({θ\_{p}}\right)}{\\,dt}\\]

\\({L\_{\text{rod}}}\\) is constant with respect to time, so

\\[{v\_{\text{x}}}={L\_{\text{rod}}} \frac{\\,d\sin\left({θ\_{p}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{x}}}=ω {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]

<div align="center">

## The \\(y\\)-component of velocity of the pendulum {#GD:velocityIY}

</div>

|Refname    |GD:velocityIY                                                                                                                                                                                                                                                                                                                                               |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of velocity of the pendulum                                                                                                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[{v\_{\text{y}}}=ω {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]                                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\({v\_{\text{y}}}\\) is the \\(y\\)-component of velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                          |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                            |

#### Detailed derivation of the \\(y\\)-component of velocity: {#GD:velocityIYDeriv}

At a given point in time, velocity may be defined as

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the vertical position

\\[{p\_{\text{y}}}=-{L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]

Applying this,

\\[{v\_{\text{y}}}=-\left(\frac{\\,d{L\_{\text{rod}}} \cos\left({θ\_{p}}\right)}{\\,dt}\right)\\]

\\({L\_{\text{rod}}}\\) is constant with respect to time, so

\\[{v\_{\text{y}}}=-{L\_{\text{rod}}} \frac{\\,d\cos\left({θ\_{p}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{y}}}=ω {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]

<div align="center">

## The \\(x\\)-component of acceleration of the pendulum {#GD:accelerationIX}

</div>

|Refname    |GD:accelerationIX                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of acceleration of the pendulum                                                                                                                                                                                                                                                                                                                                                                                                   |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[{a\_{\text{x}}}=-ω^{2} {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)+α {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]                                                                                                                                                                                                                                                                                                                                  |
|Description|<ul><li>\\({a\_{\text{x}}}\\) is the \\(x\\)-component of acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                        |

#### Detailed derivation of the \\(x\\)-component of acceleration: {#GD:accelerationIXDeriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{x}}}=ω {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{x}}}=\frac{\\,dω {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{x}}}=\frac{\\,dω}{\\,dt} {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)-ω {L\_{\text{rod}}} \sin\left({θ\_{p}}\right) \frac{\\,d{θ\_{p}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{x}}}=-ω^{2} {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)+α {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)\\]

<div align="center">

## The \\(y\\)-component of acceleration of the pendulum {#GD:accelerationIY}

</div>

|Refname    |GD:accelerationIY                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of acceleration of the pendulum                                                                                                                                                                                                                                                                                                                                                                                                   |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[{a\_{\text{y}}}=ω^{2} {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)+α {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]                                                                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\({a\_{\text{y}}}\\) is the \\(y\\)-component of acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                        |

#### Detailed derivation of the \\(y\\)-component of acceleration: {#GD:accelerationIYDeriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the vertical velocity to be

\\[{v\_{\text{y}}}=ω {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{y}}}=\frac{\\,dω {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{y}}}=\frac{\\,dω}{\\,dt} {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)+ω {L\_{\text{rod}}} \cos\left({θ\_{p}}\right) \frac{\\,d{θ\_{p}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{y}}}=ω^{2} {L\_{\text{rod}}} \cos\left({θ\_{p}}\right)+α {L\_{\text{rod}}} \sin\left({θ\_{p}}\right)\\]

<div align="center">

## Horizontal force on the pendulum {#GD:hForceOnPendulum}

</div>

|Refname    |GD:hForceOnPendulum                                                                                                                                                                                                                                                                                                                                                                     |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal force on the pendulum                                                                                                                                                                                                                                                                                                                                                        |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[\boldsymbol{F}=m {a\_{\text{x}}}=-\boldsymbol{T} \sin\left({θ\_{p}}\right)\\]                                                                                                                                                                                                                                                                                                        |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\({a\_{\text{x}}}\\) is the \\(x\\)-component of acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(\boldsymbol{T}\\) is the tension (\\({\text{N}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                        |

#### Detailed derivation of force on the pendulum: {#GD:hForceOnPendulumDeriv}

\\[\boldsymbol{F}=m {a\_{\text{x}}}=-\boldsymbol{T} \sin\left({θ\_{p}}\right)\\]

<div align="center">

## Vertical force on the pendulum {#GD:vForceOnPendulum}

</div>

|Refname    |GD:vForceOnPendulum                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical force on the pendulum                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[\boldsymbol{F}=m {a\_{\text{y}}}=\boldsymbol{T} \cos\left({θ\_{p}}\right)-m \boldsymbol{g}\\]                                                                                                                                                                                                                                                                                                                                                                                             |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\({a\_{\text{y}}}\\) is the \\(y\\)-component of acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(\boldsymbol{T}\\) is the tension (\\({\text{N}}\\))</li><li>\\({θ\_{p}}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |

#### Detailed derivation of force on the pendulum: {#GD:vForceOnPendulumDeriv}

\\[\boldsymbol{F}=m {a\_{\text{y}}}=\boldsymbol{T} \cos\left({θ\_{p}}\right)-m \boldsymbol{g}\\]

<div align="center">

## The angular frequency of the pendulum {#GD:angFrequencyGD}

</div>

|Refname    |GD:angFrequencyGD                                                                                                                                                                                                                                     |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The angular frequency of the pendulum                                                                                                                                                                                                                 |
|Units      |\\({\text{s}}\\)                                                                                                                                                                                                                                      |
|Equation   |\\[Ω=\sqrt{\frac{\boldsymbol{g}}{{L\_{\text{rod}}}}}\\]                                                                                                                                                                                               |
|Description|<ul><li>\\(Ω\\) is the angular frequency (\\({\text{s}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>The torque is defined in [TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot) and frequency is \\(f\\) is defined in [DD:frequencyDD](./SecDDs.md#DD:frequencyDD).</li></ul>                                                            |
|Source     |--                                                                                                                                                                                                                                                    |
|RefBy      |[GD:periodPend](./SecGDs.md#GD:periodPend) and [IM:calOfAngularDisplacement](./SecIMs.md#IM:calOfAngularDisplacement)                                                                                                                                 |

#### Detailed derivation of the angular frequency of the pendulum: {#GD:angFrequencyGDDeriv}

Consider the torque on a pendulum defined in [TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot). The force providing the restoring torque is the component of weight of the pendulum bob that acts along the arc length. The torque is the length of the string \\({L\_{\text{rod}}}\\) multiplied by the component of the net force that is perpendicular to the radius of the arc. The minus sign indicates the torque acts in the opposite direction of the angular displacement:

\\[\boldsymbol{τ}=-{L\_{\text{rod}}} m \boldsymbol{g} \sin\left({θ\_{p}}\right)\\]

So then

\\[\boldsymbol{I} α=-{L\_{\text{rod}}} m \boldsymbol{g} \sin\left({θ\_{p}}\right)\\]

Therefore,

\\[\boldsymbol{I} \frac{\\,d\frac{\\,d{θ\_{p}}}{\\,dt}}{\\,dt}=-{L\_{\text{rod}}} m \boldsymbol{g} \sin\left({θ\_{p}}\right)\\]

Substituting for \\(\boldsymbol{I}\\)

\\[m {L\_{\text{rod}}}^{2} \frac{\\,d\frac{\\,d{θ\_{p}}}{\\,dt}}{\\,dt}=-{L\_{\text{rod}}} m \boldsymbol{g} \sin\left({θ\_{p}}\right)\\]

Crossing out \\(m\\) and \\({L\_{\text{rod}}}\\) we have

\\[\frac{\\,d\frac{\\,d{θ\_{p}}}{\\,dt}}{\\,dt}=-\left(\frac{\boldsymbol{g}}{{L\_{\text{rod}}}}\right) \sin\left({θ\_{p}}\right)\\]

For small angles, we approximate sin \\({θ\_{p}}\\) to \\({θ\_{p}}\\)

\\[\frac{\\,d\frac{\\,d{θ\_{p}}}{\\,dt}}{\\,dt}=-\left(\frac{\boldsymbol{g}}{{L\_{\text{rod}}}}\right) {θ\_{p}}\\]

Because this equation, has the same form as the equation for simple harmonic motion the solution is easy to find.  The angular frequency

\\[Ω=\sqrt{\frac{\boldsymbol{g}}{{L\_{\text{rod}}}}}\\]

<div align="center">

## The period of the pendulum {#GD:periodPend}

</div>

|Refname    |GD:periodPend                                                                                                                                                                                                                                                                                                                  |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The period of the pendulum                                                                                                                                                                                                                                                                                                     |
|Units      |\\({\text{s}}\\)                                                                                                                                                                                                                                                                                                               |
|Equation   |\\[T=2 π \sqrt{\frac{{L\_{\text{rod}}}}{\boldsymbol{g}}}\\]                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\(T\\) is the period (\\({\text{s}}\\))</li><li>\\(π\\) is the ratio of circumference to diameter for any circle (Unitless)</li><li>\\({L\_{\text{rod}}}\\) is the length of the rod (\\({\text{m}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Notes      |<ul><li>The frequency and period are defined in the data definitions for [frequency](./SecDDs.md#DD:frequencyDD) and [period](./SecDDs.md#DD:periodSHMDD) respectively</li></ul>                                                                                                                                               |
|Source     |--                                                                                                                                                                                                                                                                                                                             |
|RefBy      |                                                                                                                                                                                                                                                                                                                               |

#### Detailed derivation of the period of the pendulum: {#GD:periodPendDeriv}

The period of the pendulum can be defined from the general definition for the equation of [angular frequency](./SecGDs.md#GD:angFrequencyGD)

\\[Ω=\sqrt{\frac{\boldsymbol{g}}{{L\_{\text{rod}}}}}\\]

Therefore from the data definition of the equation for [angular frequency](./SecDDs.md#DD:angFrequencyDD), we have

\\[T=2 π \sqrt{\frac{{L\_{\text{rod}}}}{\boldsymbol{g}}}\\]

