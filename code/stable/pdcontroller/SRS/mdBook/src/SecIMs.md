# Instance Models {#Sec:IMs}

This section transforms the problem defined in the [problem description](./SecProbDesc.md#Sec:ProbDesc) into one which is expressed in mathematical terms. It uses concrete symbols defined in the [data definitions](./SecDDs.md#Sec:DDs) to replace the abstract symbols in the models identified in [theoretical models](./SecTMs.md#Sec:TMs) and [general definitions](./SecGDs.md#Sec:GDs).

<div align="center">

## Computation of the Process Variable as a function of time {#IM:pdEquationIM}

</div>

|Refname           |IM:pdEquationIM                                                                                                                                                                                                                                                                                                      |
|:-----------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Computation of the Process Variable as a function of time                                                                                                                                                                                                                                                            |
|Input             |\\({r\_{\text{t}}}\\), \\({K\_{\text{p}}}\\), \\({K\_{\text{d}}}\\)                                                                                                                                                                                                                                                  |
|Output            |\\({y\_{\text{t}}}\\)                                                                                                                                                                                                                                                                                                |
|Input Constraints |\\[{r\_{\text{t}}}\gt{}0\\]\\[{K\_{\text{p}}}\gt{}0\\]\\[{K\_{\text{d}}}\gt{}0\\]                                                                                                                                                                                                                                    |
|Output Constraints|\\[{y\_{\text{t}}}\gt{}0\\]                                                                                                                                                                                                                                                                                          |
|Equation          |\\[\frac{\\,d^{2}{y\_{\text{t}}}}{\\,dt^{2}}+\left(1+{K\_{\text{d}}}\right)\\,\frac{\\,d{y\_{\text{t}}}}{\\,dt}+\left(20+{K\_{\text{p}}}\right)\\,{{y\_{\text{t}}}}={r\_{\text{t}}}\\,{K\_{\text{p}}}\\]                                                                                                             |
|Description       |<ul><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({y\_{\text{t}}}\\) is the Process Variable (Unitless)</li><li>\\({K\_{\text{d}}}\\) is the Derivative Gain (Unitless)</li><li>\\({K\_{\text{p}}}\\) is the Proportional Gain (Unitless)</li><li>\\({r\_{\text{t}}}\\) is the Set-Point (Unitless)</li></ul>|
|Source            |[abbasi2015](./SecReferences.md#abbasi2015) and [johnson2008](./SecReferences.md#johnson2008)                                                                                                                                                                                                                        |
|RefBy             |and                                                                                                                                                                                                                                                                                                                  |

#### Detailed derivation of Process Variable: {#IM:pdEquationIMDeriv}

The Process Variable \\({Y\_{\text{s}}}\\) in a PD Control Loop is the product of the Process Error (from [DD:ddProcessError](./SecDDs.md#DD:ddProcessError)), Control Variable (from [DD:ddCtrlVar](./SecDDs.md#DD:ddCtrlVar)), and the Power Plant (from [GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant)).

\\[{Y\_{\text{s}}}=\left({R\_{\text{s}}}-{Y\_{\text{s}}}\right)\\,\left({K\_{\text{p}}}+{K\_{\text{d}}}\\,s\right)\\,\frac{1}{s^{2}+s+20}\\]

Substituting the values and rearranging the equation.

\\[s^{2}\\,{Y\_{\text{s}}}+\left(1+{K\_{\text{d}}}\right)\\,{Y\_{\text{s}}}\\,s+\left(20+{K\_{\text{p}}}\right)\\,{Y\_{\text{s}}}-{R\_{\text{s}}}\\,s\\,{K\_{\text{d}}}-{R\_{\text{s}}}\\,{K\_{\text{p}}}=0\\]

Computing the Inverse Laplace Transform of a function (from [TM:invLaplaceTransform](./SecTMs.md#TM:invLaplaceTransform)) of the equation.

\\[\frac{\\,d\frac{\\,d{y\_{\text{t}}}}{\\,dt}}{\\,dt}+\left(1+{K\_{\text{d}}}\right)\\,\frac{\\,d{y\_{\text{t}}}}{\\,dt}+\left(20+{K\_{\text{p}}}\right)\\,{y\_{\text{t}}}-{K\_{\text{d}}}\\,\frac{\\,d{r\_{\text{t}}}}{\\,dt}-{r\_{\text{t}}}\\,{K\_{\text{p}}}=0\\]

The Set-Point \\({r\_{\text{t}}}\\) is a step function and a constant (from [A:Set-Point](./SecAssumps.md#setPointConstant)). Therefore the differential of the set point is zero. Hence the equation reduces to

\\[\frac{\\,d\frac{\\,d{y\_{\text{t}}}}{\\,dt}}{\\,dt}+\left(1+{K\_{\text{d}}}\right)\\,\frac{\\,d{y\_{\text{t}}}}{\\,dt}+\left(20+{K\_{\text{p}}}\right)\\,{y\_{\text{t}}}-{r\_{\text{t}}}\\,{K\_{\text{p}}}=0\\]
