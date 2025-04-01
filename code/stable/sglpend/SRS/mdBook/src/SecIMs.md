# Instance Models {#Sec:IMs}

This section transforms the problem defined in the [problem description](./SecProbDesc.md#Sec:ProbDesc) into one which is expressed in mathematical terms. It uses concrete symbols defined in the [data definitions](./SecDDs.md#Sec:DDs) to replace the abstract symbols in the models identified in [theoretical models](./SecTMs.md#Sec:TMs) and [general definitions](./SecGDs.md#Sec:GDs).

<div align="center">

## Calculation of angular displacement {#IM:calOfAngularDisplacement}

</div>

|Refname           |IM:calOfAngularDisplacement                                                                                                                                                                                                                                                     |
|:-----------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Calculation of angular displacement                                                                                                                                                                                                                                             |
|Input             |\\(L\_{\text{rod}}\\), \\(θ\_{i}\\), \\(\boldsymbol{g}\\)                                                                                                                                                                                                                       |
|Output            |\\(θ\_{p}\\)                                                                                                                                                                                                                                                                    |
|Input Constraints |\\[L\_{\text{rod}}\gt{}0\\]\\[θ\_{i}\gt{}0\\]\\[\boldsymbol{g}\gt{}0\\]                                                                                                                                                                                                         |
|Output Constraints|\\[θ\_{p}\gt{}0\\]                                                                                                                                                                                                                                                              |
|Equation          |\\[θ\_{p}\left(t\right)=θ\_{i}\\,\cos\left(Ω\\,t\right)\\]                                                                                                                                                                                                                      |
|Description       |<ul><li>\\(θ\_{p}\\) is the displacement angle of the pendulum (\\({\text{rad}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(θ\_{i}\\) is the initial pendulum angle (\\({\text{rad}}\\))</li><li>\\(Ω\\) is the angular frequency (\\({\text{s}}\\))</li></ul>|
|Notes             |<ul><li>The constraint \\(θ\_{i}\gt{}0\\) is required. The angular frequency is defined in [GD:angFrequencyGD](./SecGDs.md#GD:angFrequencyGD).</li></ul>                                                                                                                        |
|Source            |--                                                                                                                                                                                                                                                                              |
|RefBy             |[FR:Output-Values](./SecFRs.md#outputValues) and [FR:Calculate-Angular-Position-Of-Mass](./SecFRs.md#calcAngPos)                                                                                                                                                                |

#### Detailed derivation of angular displacement: {#IM:calOfAngularDisplacementDeriv}

When the pendulum is displaced to an initial angle and released, the pendulum swings back and forth with periodic motion. By applying [Newton's second law for rotational motion](./SecTMs.md#TM:NewtonSecLawRotMot), the equation of motion for the pendulum may be obtained:

\\[\boldsymbol{τ}=\boldsymbol{I}\\,α\\]

Where \\(\boldsymbol{τ}\\) denotes the torque, \\(\boldsymbol{I}\\) denotes the moment of inertia and \\(α\\) denotes the angular acceleration. This implies:

\\[-m\\,\boldsymbol{g}\\,\sin\left(θ\_{p}\right)\\,L\_{\text{rod}}=m\\,L\_{\text{rod}}^{2}\\,\frac{\\,d\frac{\\,dθ\_{p}}{\\,dt}}{\\,dt}\\]

And rearranged as:

\\[\frac{\\,d\frac{\\,dθ\_{p}}{\\,dt}}{\\,dt}+\frac{\boldsymbol{g}}{L\_{\text{rod}}}\\,\sin\left(θ\_{p}\right)=0\\]

If the amplitude of angular displacement is small enough, we can approximate \\(\sin\left(θ\_{p}\right)=θ\_{p}\\) for the purpose of a simple pendulum at very small angles. Then the equation of motion reduces to the equation of simple harmonic motion:

\\[\frac{\\,d\frac{\\,dθ\_{p}}{\\,dt}}{\\,dt}+\frac{\boldsymbol{g}}{L\_{\text{rod}}}\\,θ\_{p}=0\\]

Thus the simple harmonic motion is:

\\[θ\_{p}\left(t\right)=θ\_{i}\\,\cos\left(Ω\\,t\right)\\]
