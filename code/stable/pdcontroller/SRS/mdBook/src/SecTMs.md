# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that PD Controller is based on.

<div align="center">

## Laplace Transform {#TM:laplaceTransform}

</div>

|Refname    |TM:laplaceTransform                                                                                                                                                                                                                                                                      |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Laplace Transform                                                                                                                                                                                                                                                                        |
|Equation   |\\[{F\_{\text{s}}}=\int\_{\mathit{-∞}}^{∞}{{f\_{\text{t}}} e^{-s t}}\\,dt\\]                                                                                                                                                                                                             |
|Description|<ul><li>\\({F\_{\text{s}}}\\) is the Laplace Transform of a function (Unitless)</li><li>\\({f\_{\text{t}}}\\) is the Function in the time domain (Unitless)</li><li>\\(s\\) is the Complex frequency-domain parameter (Unitless)</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li></ul>|
|Notes      |<ul><li>Bilateral Laplace Transform. The Laplace transforms are typically inferred from a pre-computed table of Laplace Transforms ([laplaceWiki](./SecReferences.md#laplaceWiki)).</li></ul>                                                                                            |
|Source     |[laplaceWiki](./SecReferences.md#laplaceWiki)                                                                                                                                                                                                                                            |
|RefBy      |[GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant), [DD:ddPropCtrl](./SecDDs.md#DD:ddPropCtrl), [DD:ddProcessError](./SecDDs.md#DD:ddProcessError), and [DD:ddDerivCtrl](./SecDDs.md#DD:ddDerivCtrl)                                                                                         |

<div align="center">

## Inverse Laplace Transform {#TM:invLaplaceTransform}

</div>

|Refname    |TM:invLaplaceTransform                                                                                                                                                                                     |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Inverse Laplace Transform                                                                                                                                                                                  |
|Equation   |\\[{f\_{\text{t}}}=\mathit{L⁻¹[F(s)]}\\]                                                                                                                                                                   |
|Description|<ul><li>\\({f\_{\text{t}}}\\) is the Function in the time domain (Unitless)</li><li>\\(\mathit{L⁻¹[F(s)]}\\) is the Inverse Laplace Transform of a function (Unitless)</li></ul>                           |
|Notes      |<ul><li>Inverse Laplace Transform of F(S). The Inverse Laplace transforms are typically inferred from a pre-computed table of Laplace Transforms ([laplaceWiki](./SecReferences.md#laplaceWiki)).</li></ul>|
|Source     |[laplaceWiki](./SecReferences.md#laplaceWiki)                                                                                                                                                              |
|RefBy      |[IM:pdEquationIM](./SecIMs.md#IM:pdEquationIM)                                                                                                                                                             |

<div align="center">

## Second Order Mass-Spring-Damper System {#TM:tmSOSystem}

</div>

|Refname    |TM:tmSOSystem                                                                                                                                                                                                                                                                   |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Second Order Mass-Spring-Damper System                                                                                                                                                                                                                                          |
|Equation   |\\[\frac{1}{m s^{2}+c s+k}\\]                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(s\\) is the Complex frequency-domain parameter (Unitless)</li><li>\\(c\\) is the Damping coefficient of the spring (Unitless)</li><li>\\(k\\) is the Stiffness coefficient of the spring (\\({\text{s}}\\))</li></ul>|
|Notes      |<ul><li>The Transfer Function (from [A:Transfer Function](./SecAssumps.md#pwrPlantTxFnx)) of a Second Order System (mass-spring-damper) is characterized by this equation.</li></ul>                                                                                            |
|Source     |[abbasi2015](./SecReferences.md#abbasi2015)                                                                                                                                                                                                                                     |
|RefBy      |[GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant)                                                                                                                                                                                                                                  |
