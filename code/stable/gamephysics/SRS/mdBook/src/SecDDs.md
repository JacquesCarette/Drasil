# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Center of Mass {#DD:ctrOfMass}

</div>

|Refname    |DD:ctrOfMass                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Center of Mass                                                                                                                                                                                                                                                                                                                                                                                |
|Symbol     |\\({\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}\\)                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                                                              |
|Equation   |\\[{\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}=\frac{\displaystyle\sum{{m\_{j}}\\,{\boldsymbol{p}\text{(}t\text{)}\_{j}}}}{{m\_{T}}}\\]                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\({\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}\\) is the Center of Mass (\\({\text{m}}\\))</li><li>\\({m\_{j}}\\) is the mass of the j-th particle (\\({\text{kg}}\\))</li><li>\\({\boldsymbol{p}\text{(}t\text{)}\_{j}}\\) is the position vector of the j-th particle (\\({\text{m}}\\))</li><li>\\({m\_{T}}\\) is the total mass of the rigid body (\\({\text{kg}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>                                                                                                                                                                                                                                                                                           |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                            |
|RefBy      |[IM:transMot](./SecIMs.md#IM:transMot) and [IM:col2D](./SecIMs.md#IM:col2D)                                                                                                                                                                                                                                                                                                                   |

<div align="center">

## Linear displacement {#DD:linDisp}

</div>

|Refname    |DD:linDisp                                                                                                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear displacement                                                                                                                                                                                                     |
|Symbol     |\\(u\text{(}t\text{)}\\)                                                                                                                                                                                                |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                        |
|Equation   |\\[u\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}\left(t\right)}{\\,dt}\\]                                                                                                                                |
|Description|<ul><li>\\(u\text{(}t\text{)}\\) is the linear displacement (\\({\text{m}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>                                                                                                                     |
|Source     |--                                                                                                                                                                                                                      |
|RefBy      |[IM:transMot](./SecIMs.md#IM:transMot)                                                                                                                                                                                  |

<div align="center">

## Linear velocity {#DD:linVel}

</div>

|Refname    |DD:linVel                                                                                                                                                                                                             |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear velocity                                                                                                                                                                                                       |
|Symbol     |\\(v\text{(}t\text{)}\\)                                                                                                                                                                                              |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                       |
|Equation   |\\[v\text{(}t\text{)}=\frac{\\,d\boldsymbol{u}\left(t\right)}{\\,dt}\\]                                                                                                                                               |
|Description|<ul><li>\\(v\text{(}t\text{)}\\) is the linear velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{u}\\) is the displacement (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>                                                                                                                   |
|Source     |--                                                                                                                                                                                                                    |
|RefBy      |[IM:transMot](./SecIMs.md#IM:transMot)                                                                                                                                                                                |

<div align="center">

## Linear acceleration {#DD:linAcc}

</div>

|Refname    |DD:linAcc                                                                                                                                                                                                                                                 |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear acceleration                                                                                                                                                                                                                                       |
|Symbol     |\\(a\text{(}t\text{)}\\)                                                                                                                                                                                                                                  |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                       |
|Equation   |\\[a\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}\left(t\right)}{\\,dt}\\]                                                                                                                                                                  |
|Description|<ul><li>\\(a\text{(}t\text{)}\\) is the linear acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>                                                                                                                                                       |
|Source     |--                                                                                                                                                                                                                                                        |
|RefBy      |[IM:transMot](./SecIMs.md#IM:transMot)                                                                                                                                                                                                                    |

<div align="center">

## Angular displacement {#DD:angDisp}

</div>

|Refname    |DD:angDisp                                                                                                                                                                       |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular displacement                                                                                                                                                             |
|Symbol     |\\(θ\\)                                                                                                                                                                          |
|Units      |\\({\text{rad}}\\)                                                                                                                                                               |
|Equation   |\\[θ=\frac{\\,dϕ\left(t\right)}{\\,dt}\\]                                                                                                                                        |
|Description|<ul><li>\\(θ\\) is the angular displacement (\\({\text{rad}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(ϕ\\) is the orientation (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li></ul>     |
|Source     |--                                                                                                                                                                               |
|RefBy      |[IM:rotMot](./SecIMs.md#IM:rotMot)                                                                                                                                               |

<div align="center">

## Angular velocity {#DD:angVel}

</div>

|Refname    |DD:angVel                                                                                                                                                                                            |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular velocity                                                                                                                                                                                     |
|Symbol     |\\(ω\\)                                                                                                                                                                                              |
|Units      |\\(\frac{\text{rad}}{\text{s}}\\)                                                                                                                                                                    |
|Equation   |\\[ω=\frac{\\,dθ\left(t\right)}{\\,dt}\\]                                                                                                                                                            |
|Description|<ul><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(θ\\) is the angular displacement (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li></ul>                         |
|Source     |--                                                                                                                                                                                                   |
|RefBy      |[IM:rotMot](./SecIMs.md#IM:rotMot)                                                                                                                                                                   |

<div align="center">

## Angular acceleration {#DD:angAccel}

</div>

|Refname    |DD:angAccel                                                                                                                                                                                                             |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular acceleration                                                                                                                                                                                                    |
|Symbol     |\\(α\\)                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{rad}}{\text{s}^{2}}\\)                                                                                                                                                                                   |
|Equation   |\\[α=\frac{\\,dω\left(t\right)}{\\,dt}\\]                                                                                                                                                                               |
|Description|<ul><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li></ul>                                            |
|Source     |--                                                                                                                                                                                                                      |
|RefBy      |[IM:rotMot](./SecIMs.md#IM:rotMot)                                                                                                                                                                                      |

<div align="center">

## Chasles' theorem {#DD:chaslesThm}

</div>

|Refname    |DD:chaslesThm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Chasles' theorem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Symbol     |\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Equation   |\\[{\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}={\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}+ω\times{\boldsymbol{u}\_{\text{O}\text{B}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Description|<ul><li>\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\) is the velocity at point B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}\\) is the velocity at point origin (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({\boldsymbol{u}\_{\text{O}\text{B}}}\\) is the displacement vector between the origin and point B (\\({\text{m}}\\))</li></ul>                                                                                   |
|Notes      |<ul><li>The linear velocity \\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\) of any point B in a rigid body is the sum of the linear velocity \\({\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}\\) of the rigid body at the origin (axis of rotation) and the resultant vector from the cross product of the rigid body's angular velocity \\(ω\\) and the displacement vector between the origin and point B \\({\boldsymbol{u}\_{\text{O}\text{B}}}\\).</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>|
|Source     |[chaslesWiki](./SecReferences.md#chaslesWiki)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |

<div align="center">

## Torque {#DD:torque}

</div>

|Refname    |DD:torque                                                                                                                                                                                                        |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Torque                                                                                                                                                                                                           |
|Symbol     |\\(\boldsymbol{τ}\\)                                                                                                                                                                                             |
|Units      |\\(\text{N}\text{m}\\)                                                                                                                                                                                           |
|Equation   |\\[\boldsymbol{τ}=\boldsymbol{r}\times\boldsymbol{F}\\]                                                                                                                                                          |
|Description|<ul><li>\\(\boldsymbol{τ}\\) is the torque (\\(\text{N}\text{m}\\))</li><li>\\(\boldsymbol{r}\\) is the position vector (\\({\text{m}}\\))</li><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li></ul>|
|Notes      |<ul><li>The torque on a body measures the tendency of a force to rotate the body around an axis or pivot.</li></ul>                                                                                              |
|Source     |--                                                                                                                                                                                                               |
|RefBy      |                                                                                                                                                                                                                 |

<div align="center">

## Kinetic energy {#DD:kEnergy}

</div>

|Refname    |DD:kEnergy                                                                                                                                                                                                                                                                                                                                                                   |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Kinetic energy                                                                                                                                                                                                                                                                                                                                                               |
|Symbol     |\\(KE\\)                                                                                                                                                                                                                                                                                                                                                                     |
|Units      |\\({\text{J}}\\)                                                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[KE=m\\,\frac{\|\boldsymbol{v}\text{(}t\text{)}\|^{2}}{2}\\]                                                                                                                                                                                                                                                                                                               |
|Description|<ul><li>\\(KE\\) is the kinetic energy (\\({\text{J}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>                                                                                                                                                          |
|Notes      |<ul><li>Kinetic energy is the measure of the energy a body possesses due to its motion.</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li><li>No damping occurs during the simulation (from [A:dampingInvolvement](./SecAssumps.md#assumpDI)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                             |

<div align="center">

## Coefficient of restitution {#DD:coeffRestitution}

</div>

|Refname    |DD:coeffRestitution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Coefficient of restitution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Symbol     |\\({C\_{\text{R}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Units      |Unitless                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{C\_{\text{R}}}=-\left(\frac{{{\boldsymbol{v}\text{(}t\text{)}\_{\text{f}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}{{{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}\right)\\]                                                                                                                                                                                                                                                                                                              |
|Description|<ul><li>\\({C\_{\text{R}}}\\) is the coefficient of restitution (Unitless)</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{f}}}^{\text{A}\text{B}}}\\) is the final relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(\boldsymbol{n}\\) is the collision normal vector (\\({\text{m}}\\))</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>The coefficient of restitution \\({C\_{\text{R}}}\\) determines the elasticity of a collision between two rigid bodies. \\({C\_{\text{R}}}=1\\) results in an elastic collision, \\({C\_{\text{R}}}\lt{}1\\) results in an inelastic collision, and \\({C\_{\text{R}}}=0\\) results in a totally inelastic collision.</li></ul>                                                                                                                                                                                                  |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |

<div align="center">

## Initial Relative Velocity Between Rigid Bodies of A and B {#DD:reVeInColl}

</div>

|Refname    |DD:reVeInColl                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Initial Relative Velocity Between Rigid Bodies of A and B                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Symbol     |\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}={\boldsymbol{v}\text{(}t\text{)}^{\text{A}\text{P}}}-{\boldsymbol{v}\text{(}t\text{)}^{\text{B}\text{P}}}\\]                                                                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}^{\text{A}\text{P}}}\\) is the velocity of the point of collision P in body A (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}^{\text{B}\text{P}}}\\) is the velocity of the point of collision P in body B (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>In a collision, the velocity of a rigid body A colliding with another rigid body B relative to that body \\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the difference between the velocities of A and B at point P.</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>                                                                                                                                                      |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

<div align="center">

## Impulse (vector) {#DD:impulseV}

</div>

|Refname    |DD:impulseV                                                                                                                                                                                                                             |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Impulse (vector)                                                                                                                                                                                                                        |
|Symbol     |\\(\boldsymbol{J}\\)                                                                                                                                                                                                                    |
|Units      |\\(\text{N}\text{s}\\)                                                                                                                                                                                                                  |
|Equation   |\\[\boldsymbol{J}=m\\,Δ\boldsymbol{v}\\]                                                                                                                                                                                                |
|Description|<ul><li>\\(\boldsymbol{J}\\) is the impulse (vector) (\\(\text{N}\text{s}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(Δ\boldsymbol{v}\\) is the change in velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>       |
|Notes      |<ul><li>An impulse (vector) \\(\boldsymbol{J}\\) occurs when a force \\(\boldsymbol{F}\\) acts over a body over an interval of time.</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                      |
|RefBy      |                                                                                                                                                                                                                                        |

#### Detailed derivation of impulse (vector): {#DD:impulseVDeriv}

Newton's second law of motion states:

\\[\boldsymbol{F}=m\\,\boldsymbol{a}\text{(}t\text{)}=m\\,\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Rearranging:

\\[\int\_{{t\_{1}}}^{{t\_{2}}}{\boldsymbol{F}}\\,dt=m\\,\left(\int\_{{\boldsymbol{v}\text{(}t\text{)}\_{1}}}^{{\boldsymbol{v}\text{(}t\text{)}\_{2}}}{1}\\,d\boldsymbol{v}\text{(}t\text{)}\right)\\]

Integrating the right hand side:

\\[\int\_{{t\_{1}}}^{{t\_{2}}}{\boldsymbol{F}}\\,dt=m\\,{\boldsymbol{v}\text{(}t\text{)}\_{2}}-m\\,{\boldsymbol{v}\text{(}t\text{)}\_{1}}=m\\,Δ\boldsymbol{v}\\]

<div align="center">

## Potential energy {#DD:potEnergy}

</div>

|Refname    |DD:potEnergy                                                                                                                                                                                                                                                                                                                                                                                             |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Potential energy                                                                                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\(PE\\)                                                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\({\text{J}}\\)                                                                                                                                                                                                                                                                                                                                                                                         |
|Equation   |\\[PE=m\\,\boldsymbol{g}\\,h\\]                                                                                                                                                                                                                                                                                                                                                                          |
|Description|<ul><li>\\(PE\\) is the potential energy (\\({\text{J}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(h\\) is the height (\\({\text{m}}\\))</li></ul>                                                                                                                              |
|Notes      |<ul><li>The potential energy of an object is the energy held by an object because of its position to other objects.</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li><li>No damping occurs during the simulation (from [A:dampingInvolvement](./SecAssumps.md#assumpDI)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                       |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                         |

<div align="center">

## Moment of inertia {#DD:momentOfInertia}

</div>

|Refname    |DD:momentOfInertia                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Moment of inertia                                                                                                                                                                                                                                                                     |
|Symbol     |\\(\boldsymbol{I}\\)                                                                                                                                                                                                                                                                  |
|Units      |\\(\text{kg}\text{m}^{2}\\)                                                                                                                                                                                                                                                           |
|Equation   |\\[\boldsymbol{I}=\displaystyle\sum{{m\_{j}}\\,{d\_{j}}^{2}}\\]                                                                                                                                                                                                                       |
|Description|<ul><li>\\(\boldsymbol{I}\\) is the moment of inertia (\\(\text{kg}\text{m}^{2}\\))</li><li>\\({m\_{j}}\\) is the mass of the j-th particle (\\({\text{kg}}\\))</li><li>\\({d\_{j}}\\) is the distance between the j-th particle and the axis of rotation (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>The moment of inertia \\(\boldsymbol{I}\\) of a body measures how much torque is needed for the body to achieve angular acceleration about the axis of rotation.</li><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)).</li></ul>          |
|Source     |--                                                                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                                                                      |
