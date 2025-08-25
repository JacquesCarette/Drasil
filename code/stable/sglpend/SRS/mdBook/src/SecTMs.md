# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that SglPend is based on.

<div align="center">

## Acceleration {#TM:acceleration}

</div>

|Refname    |TM:acceleration                                                                                                                                                                                                                                                 |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration                                                                                                                                                                                                                                                    |
|Equation   |\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |[accelerationWiki](./SecReferences.md#accelerationWiki)                                                                                                                                                                                                         |
|RefBy      |                                                                                                                                                                                                                                                                |

<div align="center">

## Velocity {#TM:velocity}

</div>

|Refname    |TM:velocity                                                                                                                                                                                                                              |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity                                                                                                                                                                                                                                 |
|Equation   |\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                  |
|Description|<ul><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Source     |[velocityWiki](./SecReferences.md#velocityWiki)                                                                                                                                                                                          |
|RefBy      |                                                                                                                                                                                                                                         |

<div align="center">

## Newton's second law of motion {#TM:NewtonSecLawMot}

</div>

|Refname    |TM:NewtonSecLawMot                                                                                                                                                                                                                  |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's second law of motion                                                                                                                                                                                                       |
|Equation   |\\[\boldsymbol{F}=m\\,\boldsymbol{a}\text{(}t\text{)}\\]                                                                                                                                                                            |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>      |
|Notes      |<ul><li>The net force \\(\boldsymbol{F}\\) on a body is proportional to the acceleration \\(\boldsymbol{a}\text{(}t\text{)}\\) of the body, where \\(m\\) denotes the mass of the body as the constant of proportionality.</li></ul>|
|Source     |--                                                                                                                                                                                                                                  |
|RefBy      |                                                                                                                                                                                                                                    |

<div align="center">

## Newton's second law for rotational motion {#TM:NewtonSecLawRotMot}

</div>

|Refname    |TM:NewtonSecLawRotMot                                                                                                                                                                                                                                |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's second law for rotational motion                                                                                                                                                                                                            |
|Equation   |\\[\boldsymbol{τ}=\boldsymbol{I}\\,α\\]                                                                                                                                                                                                              |
|Description|<ul><li>\\(\boldsymbol{τ}\\) is the torque (\\(\text{N}\text{m}\\))</li><li>\\(\boldsymbol{I}\\) is the moment of inertia (\\(\text{kg}\text{m}^{2}\\))</li><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Notes      |<ul><li>The net torque \\(\boldsymbol{τ}\\) on a rigid body is proportional to its angular acceleration \\(α\\), where \\(\boldsymbol{I}\\) denotes the moment of inertia of the rigid body as the constant of proportionality.</li></ul>            |
|Source     |--                                                                                                                                                                                                                                                   |
|RefBy      |[IM:calOfAngularDisplacement](./SecIMs.md#IM:calOfAngularDisplacement) and [GD:angFrequencyGD](./SecGDs.md#GD:angFrequencyGD)                                                                                                                        |
