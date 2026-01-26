# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Minimum thickness {#DD:minThick}

</div>

|Refname    |DD:minThick                                                                                                                                                                                                                                                       |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Minimum thickness                                                                                                                                                                                                                                                 |
|Symbol     |\\(h\\)                                                                                                                                                                                                                                                           |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                  |
|Equation   |\\[h=\frac{1}{1000}\\,\begin{cases}2.16, & t=2.5\\\\2.59, & t=2.7\\\\2.92, & t=3.0\\\\3.78, & t=4.0\\\\4.57, & t=5.0\\\\5.56, & t=6.0\\\\7.42, & t=8.0\\\\9.02, & t=10.0\\\\11.91, & t=12.0\\\\15.09, & t=16.0\\\\18.26, & t=19.0\\\\21.44, & t=22.0\end{cases}\\]|
|Description|<ul><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(t\\) is the nominal thickness (\\({\text{mm}}\\))</li></ul>                                                                                                                                |
|Notes      |<ul><li>\\(t\\) is a function that maps from the nominal thickness (\\(h\\)) to the minimum thickness.</li><li>nominal thickness t is in \\(\{2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0\}\\)</li></ul>                                                 |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                                                           |
|RefBy      |[IM:sdfTol](./SecIMs.md#IM:sdfTol), [IM:riskFun](./SecIMs.md#IM:riskFun), [IM:nFL](./SecIMs.md#IM:nFL), and [IM:dimlessLoad](./SecIMs.md#IM:dimlessLoad)                                                                                                          |

<div align="center">

## Load duration factor {#DD:loadDurFactor}

</div>

|Refname    |DD:loadDurFactor                                                                                                                                                                                                                             |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Load duration factor                                                                                                                                                                                                                         |
|Symbol     |\\(\mathit{LDF}\\)                                                                                                                                                                                                                           |
|Units      |Unitless                                                                                                                                                                                                                                     |
|Equation   |\\[\mathit{LDF}=\left(\frac{{t\_{\text{d}}}}{60}\right)^{\frac{m}{16}}\\]                                                                                                                                                                    |
|Description|<ul><li>\\(\mathit{LDF}\\) is the load duration factor (Unitless)</li><li>\\({t\_{\text{d}}}\\) is the duration of load (\\({\text{s}}\\))</li><li>\\(m\\) is the surface flaw parameter (\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\))</li></ul>|
|Notes      |<ul><li>\\({t\_{\text{d}}}\\) and \\(m\\) come from [A:standardValues](./SecAssumps.md#assumpSV).</li><li>\\(\mathit{LDF}\\) is assumed to be constant (from [A:ldfConstant](./SecAssumps.md#assumpLDFC)).</li></ul>                         |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                                      |
|RefBy      |[IM:sdfTol](./SecIMs.md#IM:sdfTol) and [IM:riskFun](./SecIMs.md#IM:riskFun)                                                                                                                                                                  |

<div align="center">

## Glass type factor {#DD:gTF}

</div>

|Refname    |DD:gTF                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------|
|Label      |Glass type factor                                                                                                                   |
|Symbol     |\\(\mathit{GTF}\\)                                                                                                                  |
|Units      |Unitless                                                                                                                            |
|Equation   |\\[\mathit{GTF}=\begin{cases}1, & g=\text{\\(\``\\)AN''}\\\\4, & g=\text{\\(\``\\)FT''}\\\\2, & g=\text{\\(\``\\)HS''}\end{cases}\\]|
|Description|<ul><li>\\(\mathit{GTF}\\) is the glass type factor (Unitless)</li><li>\\(g\\) is the glass type (Unitless)</li></ul>               |
|Notes      |<ul><li>AN is annealed glass.</li><li>FT is fully tempered glass.</li><li>HS is heat strengthened glass.</li></ul>                  |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                             |
|RefBy      |[IM:calofCapacity](./SecIMs.md#IM:calofCapacity) and [IM:dimlessLoad](./SecIMs.md#IM:dimlessLoad)                                   |

<div align="center">

## Stand off distance {#DD:standOffDist}

</div>

|Refname    |DD:standOffDist                                                                                                                                                                                                                                                                                                                                                                                            |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Stand off distance                                                                                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\(\mathit{SD}\\)                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                                                                           |
|Equation   |\\[\mathit{SD}=\sqrt{{\mathit{SD}\_{\text{x}}}^{2}+{\mathit{SD}\_{\text{y}}}^{2}+{\mathit{SD}\_{\text{z}}}^{2}}\\]                                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\(\mathit{SD}\\) is the stand off distance (\\({\text{m}}\\))</li><li>\\({\mathit{SD}\_{\text{x}}}\\) is the stand off distance (\\(x\\)-component) (\\({\text{m}}\\))</li><li>\\({\mathit{SD}\_{\text{y}}}\\) is the stand off distance (\\(y\\)-component) (\\({\text{m}}\\))</li><li>\\({\mathit{SD}\_{\text{z}}}\\) is the stand off distance (\\(z\\)-component) (\\({\text{m}}\\))</li></ul>|
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                           |

<div align="center">

## Aspect ratio {#DD:aspectRatio}

</div>

|Refname    |DD:aspectRatio                                                                                                                                                                                                    |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Aspect ratio                                                                                                                                                                                                      |
|Symbol     |\\(\mathit{AR}\\)                                                                                                                                                                                                 |
|Units      |Unitless                                                                                                                                                                                                          |
|Equation   |\\[\mathit{AR}=\frac{a}{b}\\]                                                                                                                                                                                     |
|Description|<ul><li>\\(\mathit{AR}\\) is the aspect ratio (Unitless)</li><li>\\(a\\) is the plate length (long dimension) (\\({\text{m}}\\))</li><li>\\(b\\) is the plate width (short dimension) (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>\\(a\\) and \\(b\\) are the dimensions of the plate, where (\\(a\geq{}b\\)).</li></ul>                                                                                                                    |
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                  |

<div align="center">

## Equivalent TNT charge mass {#DD:eqTNTW}

</div>

|Refname    |DD:eqTNTW                                                                                                                                                                                                                    |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Equivalent TNT charge mass                                                                                                                                                                                                   |
|Symbol     |\\({w\_{\mathit{TNT}}}\\)                                                                                                                                                                                                    |
|Units      |\\({\text{kg}}\\)                                                                                                                                                                                                            |
|Equation   |\\[{w\_{\mathit{TNT}}}=w\\,\mathit{TNT}\\]                                                                                                                                                                                   |
|Description|<ul><li>\\({w\_{\mathit{TNT}}}\\) is the equivalent TNT charge mass (\\({\text{kg}}\\))</li><li>\\(w\\) is the charge weight (\\({\text{kg}}\\))</li><li>\\(\mathit{TNT}\\) is the TNT equivalent factor (Unitless)</li></ul>|
|Source     |[astm2009](./SecReferences.md#astm2009)                                                                                                                                                                                      |
|RefBy      |                                                                                                                                                                                                                             |
