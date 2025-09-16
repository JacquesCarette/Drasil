# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that SSP is based on.

<div align="center">

## Factor of safety {#TM:factOfSafety}

</div>

|Refname    |TM:factOfSafety                                                                                                                                                                                           |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Factor of safety                                                                                                                                                                                          |
|Equation   |\\[{F\_{\text{S}}}=\frac{P}{S}\\]                                                                                                                                                                         |
|Description|<ul><li>\\({F\_{\text{S}}}\\) is the factor of safety (Unitless)</li><li>\\(P\\) is the resistive shear force (\\({\text{N}}\\))</li><li>\\(S\\) is the mobilized shear force (\\({\text{N}}\\))</li></ul>|
|Source     |[fredlund1977](./SecReferences.md#fredlund1977)                                                                                                                                                           |
|RefBy      |[GD:mobShr](./SecGDs.md#GD:mobShr)                                                                                                                                                                        |

<div align="center">

## Equilibrium {#TM:equilibrium}

</div>

|Refname    |TM:equilibrium                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Equilibrium                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[\displaystyle\sum{{F\_{\text{x}}}}=0\\]\\[\displaystyle\sum{{F\_{\text{y}}}}=0\\]\\[\displaystyle\sum{M}=0\\]                                                                                                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\({F\_{\text{x}}}\\) is the \\(x\\)-coordinate of the force (\\({\text{N}}\\))</li></ul><ul><li>\\({F\_{\text{y}}}\\) is the \\(y\\)-coordinate of the force (\\({\text{N}}\\))</li></ul><ul><li>\\(M\\) is the moment (\\(\text{N}\text{m}\\))</li></ul>                                                                                                                                                                                              |
|Notes      |<ul><li>For a body in static equilibrium, the net forces and moments acting on the body will cancel out. Assuming a 2D problem ([A:Effective-Norm-Stress-Large](./SecAssumps.md#assumpENSL)), the \\(x\\)-coordinate of the force \\({F\_{\text{x}}}\\) and \\(y\\)-coordinate of the force \\({F\_{\text{y}}}\\) will be equal to \\(0\\). All forces and their distance from the chosen point of rotation will create a net moment equal to \\(0\\).</li></ul>|
|Source     |[fredlund1977](./SecReferences.md#fredlund1977)                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |[GD:normForcEq](./SecGDs.md#GD:normForcEq), [GD:momentEql](./SecGDs.md#GD:momentEql), and [GD:bsShrFEq](./SecGDs.md#GD:bsShrFEq)                                                                                                                                                                                                                                                                                                                                |

<div align="center">

## Mohr-Coulumb shear strength {#TM:mcShrStrgth}

</div>

|Refname    |TM:mcShrStrgth                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Mohr-Coulumb shear strength                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Equation   |\\[{τ^{\text{f}}}={σ\_{N}}'\\,\tan\left(φ'\right)+c'\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Description|<ul><li>\\({τ^{\text{f}}}\\) is the shear strength (\\({\text{Pa}}\\))</li><li>\\({σ\_{N}}'\\) is the effective normal stress (\\({\text{Pa}}\\))</li><li>\\(φ'\\) is the effective angle of friction (\\({{}^{\circ}}\\))</li><li>\\(c'\\) is the effective cohesion (\\({\text{Pa}}\\))</li></ul>                                                                                                                                                                                                                                                                                                                               |
|Notes      |<ul><li>In this model the shear strength \\({τ^{\text{f}}}\\) is proportional to the product of the effective normal stress \\({σ\_{N}}'\\) on the plane with its static friction in the angular form \\(\tan\left(φ'\right)\\). The \\({τ^{\text{f}}}\\) versus \\({σ\_{N}}'\\) relationship is not truly linear, but assuming the effective normal forces is strong enough, it can be approximated with a linear fit ([A:Surface-Base-Slice-between-Interslice-Straight-Lines](./SecAssumps.md#assumpSBSBISL)) where the effective cohesion \\(c'\\) represents the \\({τ^{\text{f}}}\\) intercept of the fitted line.</li></ul>|
|Source     |[fredlund1977](./SecReferences.md#fredlund1977)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|RefBy      |[GD:resShr](./SecGDs.md#GD:resShr)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

<div align="center">

## Effective stress {#TM:effStress}

</div>

|Refname    |TM:effStress                                                                                                                                                                                  |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Effective stress                                                                                                                                                                              |
|Equation   |\\[σ'=σ-u\\]                                                                                                                                                                                  |
|Description|<ul><li>\\(σ'\\) is the effective stress (\\({\text{Pa}}\\))</li><li>\\(σ\\) is the total normal stress (\\({\text{Pa}}\\))</li><li>\\(u\\) is the pore pressure (\\({\text{Pa}}\\))</li></ul>|
|Notes      |<ul><li>\\(σ\\) is defined in [DD:normStress](./SecDDs.md#DD:normStress).</li></ul>                                                                                                           |
|Source     |[fredlund1977](./SecReferences.md#fredlund1977)                                                                                                                                               |
|RefBy      |[GD:effNormF](./SecGDs.md#GD:effNormF)                                                                                                                                                        |

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
|RefBy      |[GD:weight](./SecGDs.md#GD:weight)                                                                                                                                                                                                  |
