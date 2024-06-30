# Software Requirements Specification for HGHC
W. Spencer Smith

# Table of Contents {#Sec:ToC}

An outline of all sections included in this SRS is recorded here for easy reference.

- [Table of Contents](#Sec:ToC)
- [Reference Material](#Sec:RefMat)
  - [Table of Units](#Sec:ToU)
  - [Table of Symbols](#Sec:ToS)
- [Specific System Description](#Sec:SpecSystDesc)
  - [Solution Characteristics Specification](#Sec:SolCharSpec)
    - [Theoretical Models](#Sec:TMs)
    - [General Definitions](#Sec:GDs)
    - [Data Definitions](#Sec:DDs)
    - [Instance Models](#Sec:IMs)

# Reference Material {#Sec:RefMat}

This section records information for easy reference.

# Table of Units {#Sec:ToU}

The unit system used throughout is SI (Système International d'Unités). In addition to the basic units, several derived units are also used. For each unit, the [Table of Units](#Table:ToU) lists the symbol, a description, and the SI name.

<div id="Table:ToU"></div>

|Symbol                    |Description|SI Name   |
|:-------------------------|:----------|:---------|
|\\({{}^{\circ}\text{C}}\\)|temperature|centigrade|
|\\({\text{m}}\\)          |length     |metre     |
|\\({\text{W}}\\)          |power      |watt      |

**<p align="center">Table of Units</p>**

# Table of Symbols {#Sec:ToS}

The symbols used in this document are summarized in the [Table of Symbols](#Table:ToS) along with their units. The choice of symbols was made to be consistent with the nuclear physics literature and with that used in the FP manual.

<div id="Table:ToS"></div>

|Symbol               |Description                                                      |Units                                                |
|:--------------------|:----------------------------------------------------------------|:----------------------------------------------------|
|\\({h\_{\text{b}}}\\)|Initial coolant film conductance                                 |--                                                   |
|\\({h\_{\text{c}}}\\)|Convective heat transfer coefficient between clad and coolant    |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|
|\\({h\_{\text{g}}}\\)|Effective heat transfer coefficient between clad and fuel surface|\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|
|\\({h\_{\text{p}}}\\)|Initial gap film conductance                                     |--                                                   |
|\\({k\_{\text{c}}}\\)|Clad conductivity                                                |--                                                   |
|\\({τ\_{\text{c}}}\\)|Clad thickness                                                   |--                                                   |

**<p align="center">Table of Symbols</p>**

# Specific System Description {#Sec:SpecSystDesc}

This section first presents the problem description, which gives a high-level view of the problem to be solved. This is followed by the solution characteristics specification, which presents the assumptions, theories, and definitions that are used.

# Solution Characteristics Specification {#Sec:SolCharSpec}

The instance models that govern HGHC are presented in the [Instance Model Section](#Sec:IMs). The information to understand the meaning of the instance models and their derivation is also presented, so that the instance models can be verified.

# Theoretical Models {#Sec:TMs}

There are no theoretical models.

# General Definitions {#Sec:GDs}

There are no general definitions.

# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Effective heat transfer coefficient between clad and fuel surface {#DD:htTransCladFuel}

</div>

|Refname    |DD:htTransCladFuel                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Effective heat transfer coefficient between clad and fuel surface                                                                                                                                                                                                                                                                                                                     |
|Symbol     |\\({h\_{\text{g}}}\\)                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{h\_{\text{g}}}=\frac{2 {k\_{\text{c}}} {h\_{\text{p}}}}{2 {k\_{\text{c}}}+{τ\_{\text{c}}} {h\_{\text{p}}}}\\]                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({h\_{\text{g}}}\\) is the effective heat transfer coefficient between clad and fuel surface (\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\))</li><li>\\({k\_{\text{c}}}\\) is the clad conductivity (Unitless)</li><li>\\({h\_{\text{p}}}\\) is the initial gap film conductance (Unitless)</li><li>\\({τ\_{\text{c}}}\\) is the clad thickness (Unitless)</li></ul>|

<div align="center">

## Convective heat transfer coefficient between clad and coolant {#DD:htTransCladCool}

</div>

|Refname    |DD:htTransCladCool                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Convective heat transfer coefficient between clad and coolant                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\({h\_{\text{c}}}\\)                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{h\_{\text{c}}}=\frac{2 {k\_{\text{c}}} {h\_{\text{b}}}}{2 {k\_{\text{c}}}+{τ\_{\text{c}}} {h\_{\text{b}}}}\\]                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\({h\_{\text{c}}}\\) is the convective heat transfer coefficient between clad and coolant (\\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\))</li><li>\\({k\_{\text{c}}}\\) is the clad conductivity (Unitless)</li><li>\\({h\_{\text{b}}}\\) is the initial coolant film conductance (Unitless)</li><li>\\({τ\_{\text{c}}}\\) is the clad thickness (Unitless)</li></ul>|

# Instance Models {#Sec:IMs}

There are no instance models.

