# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## Acceleration due to gravity {#GD:accelGravity}

</div>

|Refname    |GD:accelGravity                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration due to gravity                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Equation   |\\[\boldsymbol{g}=-\frac{G\\,M}{\|\boldsymbol{d}\|^{2}}\\,\boldsymbol{\hat{d}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(G\\) is the gravitational constant (\\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\))</li><li>\\(M\\) is the mass of the larger rigid body (\\({\text{kg}}\\))</li><li>\\(\|\boldsymbol{d}\|\\) is the Euclidean norm of the distance between the center of mass of two bodies (\\({\text{m}}\\))</li><li>\\(\boldsymbol{\hat{d}}\\) is the unit vector directed from the center of the large mass to the center of the smaller mass (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>If one of the masses is much larger than the other, it is convenient to define a gravitational field around the larger mass as shown above. The negative sign in the equation indicates that the force is an attractive force.</li></ul>                                                                                                                                                                                                                                                                                                                                 |
|Source     |[Definition of Gravitational Acceleration](https://en.wikipedia.org/wiki/Gravitational_acceleration)                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|RefBy      |[IM:transMot](./SecIMs.md#IM:transMot)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |

#### Detailed derivation of gravitational acceleration: {#GD:accelGravityDeriv}

From [Newton's law of universal gravitation](./SecTMs.md#TM:UniversalGravLaw), we have:

\\[\boldsymbol{F}=\frac{G\\,{m\_{1}}\\,{m\_{2}}}{{\|\boldsymbol{d}\|^{2}}}\\,\boldsymbol{\hat{d}}\\]

The above equation governs the gravitational attraction between two bodies. Suppose that one of the bodies is significantly more massive than the other, so that we concern ourselves with the force the massive body exerts on the lighter body. Further, suppose that the Cartesian coordinate system is chosen such that this force acts on a line which lies along one of the principal axes. Then our unit vector directed from the center of the large mass to the center of the smaller mass \\(\boldsymbol{\hat{d}}\\) for the x or y axes is:

\\[\boldsymbol{\hat{d}}=\frac{\boldsymbol{d}}{\|\boldsymbol{d}\|}\\]

Given the above assumptions, let \\(M\\) and \\(m\\) be the mass of the massive and light body respectively. Equating \\(\boldsymbol{F}\\) above with Newton's second law for the force experienced by the light body, we get:

\\[{\boldsymbol{F}\_{\boldsymbol{g}}}=G\\,\frac{M\\,m}{{\|\boldsymbol{d}\|^{2}}}\\,\boldsymbol{\hat{d}}=m\\,\boldsymbol{g}\\]

where \\(\boldsymbol{g}\\) is the gravitational acceleration. Dividing the above equation by \\(m\\),  we have:

\\[G\\,\frac{M}{{\|\boldsymbol{d}\|^{2}}}\\,\boldsymbol{\hat{d}}=\boldsymbol{g}\\]

and thus the negative sign indicates that the force is an attractive force:

\\[\boldsymbol{g}=-G\\,\frac{M}{{\|\boldsymbol{d}\|^{2}}}\\,\boldsymbol{\hat{d}}\\]

<div align="center">

## Impulse for Collision {#GD:impulse}

</div>

|Refname    |GD:impulse                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Impulse for Collision                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\(\text{N}\text{s}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[j=\frac{-\left(1+{C\_{\text{R}}}\right)\\,{{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}{\left(\frac{1}{{m\_{\text{A}}}}+\frac{1}{{m\_{\text{B}}}}\right)\\,\|\boldsymbol{n}\|^{2}+\frac{\|{\boldsymbol{u}\_{\text{A}\text{P}}}\text{\*}\boldsymbol{n}\|^{2}}{{\boldsymbol{I}\_{\text{A}}}}+\frac{\|{\boldsymbol{u}\_{\text{B}\text{P}}}\text{\*}\boldsymbol{n}\|^{2}}{{\boldsymbol{I}\_{\text{B}}}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Description|<ul><li>\\(j\\) is the impulse (scalar) (\\(\text{N}\text{s}\\))</li><li>\\({C\_{\text{R}}}\\) is the coefficient of restitution (Unitless)</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(\boldsymbol{n}\\) is the collision normal vector (\\({\text{m}}\\))</li><li>\\({m\_{\text{A}}}\\) is the mass of rigid body A (\\({\text{kg}}\\))</li><li>\\({m\_{\text{B}}}\\) is the mass of rigid body B (\\({\text{kg}}\\))</li><li>\\(\|\boldsymbol{n}\|\\) is the length of the normal vector (\\({\text{m}}\\))</li><li>\\(\|{\boldsymbol{u}\_{\text{A}\text{P}}}\text{\*}\boldsymbol{n}\|\\) is the length of the perpendicular vector to the contact displacement vector of rigid body A (\\({\text{m}}\\))</li><li>\\({\boldsymbol{I}\_{\text{A}}}\\) is the moment of inertia of rigid body A (\\(\text{kg}\text{m}^{2}\\))</li><li>\\(\|{\boldsymbol{u}\_{\text{B}\text{P}}}\text{\*}\boldsymbol{n}\|\\) is the length of the perpendicular vector to the contact displacement vector of rigid body B (\\({\text{m}}\\))</li><li>\\({\boldsymbol{I}\_{\text{B}}}\\) is the moment of inertia of rigid body B (\\(\text{kg}\text{m}^{2}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](./SecAssumps.md#assumpOT)) and two-dimensional (from [A:objectDimension](./SecAssumps.md#assumpOD)).</li><li>A right-handed coordinate system is used (from [A:axesDefined](./SecAssumps.md#assumpAD)).</li><li>All collisions are vertex-to-edge (from [A:collisionType](./SecAssumps.md#assumpCT)).</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Source     |[Impulse for Collision Ref](http://www.chrishecker.com/images/e/e7/Gdmphys3.pdf)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |[IM:col2D](./SecIMs.md#IM:col2D)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |