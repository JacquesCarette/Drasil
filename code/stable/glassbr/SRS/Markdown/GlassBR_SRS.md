# Software Requirements Specification for GlassBR

Nikitha Krithnan and W. Spencer Smith

# Table of Contents {#Sec:ToC}

An outline of all sections included in this SRS is recorded here for easy reference.

- [Table of Contents](#Sec:ToC)
- [Reference Material](#Sec:RefMat)
  - [Table of Units](#Sec:ToU)
  - [Table of Symbols](#Sec:ToS)
  - [Abbreviations and Acronyms](#Sec:TAbbAcc)
- [Introduction](#Sec:Intro)
  - [Purpose of Document](#Sec:DocPurpose)
  - [Scope of Requirements](#Sec:ReqsScope)
  - [Characteristics of Intended Reader](#Sec:ReaderChars)
  - [Organization of Document](#Sec:DocOrg)
- [Stakeholders](#Sec:Stakeholder)
  - [The Customer](#Sec:Customer)
  - [The Client](#Sec:Client)
- [General System Description](#Sec:GenSysDesc)
  - [System Context](#Sec:SysContext)
  - [User Characteristics](#Sec:UserChars)
  - [System Constraints](#Sec:SysConstraints)
- [Specific System Description](#Sec:SpecSystDesc)
  - [Problem Description](#Sec:ProbDesc)
    - [Physical System Description](#Sec:PhysSyst)
    - [Goal Statements](#Sec:GoalStmt)
  - [Solution Characteristics Specification](#Sec:SolCharSpec)
    - [Assumptions](#Sec:Assumps)
    - [Theoretical Models](#Sec:TMs)
    - [General Definitions](#Sec:GDs)
    - [Data Definitions](#Sec:DDs)
    - [Instance Models](#Sec:IMs)
    - [Data Constraints](#Sec:DataConstraints)
    - [Properties of a Correct Solution](#Sec:CorSolProps)
- [Requirements](#Sec:Requirements)
  - [Functional Requirements](#Sec:FRs)
  - [Non-Functional Requirements](#Sec:NFRs)
- [Likely Changes](#Sec:LCs)
- [Unlikely Changes](#Sec:UCs)
- [Traceability Matrices and Graphs](#Sec:TraceMatrices)
- [Values of Auxiliary Constants](#Sec:AuxConstants)
- [References](#Sec:References)
- [Appendix](#Sec:Appendix)

# Reference Material {#Sec:RefMat}

This section records information for easy reference.

# Table of Units {#Sec:ToU}

The unit system used throughout is SI (Système International d'Unités). In addition to the basic units, several derived units are also used. For each unit, the [Table of Units](#Table:ToU) lists the symbol, a description, and the SI name.

<div id="Table:ToU"></div>

|Symbol           |Description|SI Name |
|:----------------|:----------|:-------|
|\\({\text{kg}}\\)|mass       |kilogram|
|\\({\text{m}}\\) |length     |metre   |
|\\({\text{N}}\\) |force      |newton  |
|\\({\text{Pa}}\\)|pressure   |pascal  |
|\\({\text{s}}\\) |time       |second  |

**<p align="center">Table of Units</p>**

# Table of Symbols {#Sec:ToS}

The symbols used in this document are summarized in the [Table of Symbols](#Table:ToS) along with their units. The symbols are listed in alphabetical order.

<div id="Table:ToS"></div>

|Symbol                           |Description                                                                           |Units                                   |
|:--------------------------------|:-------------------------------------------------------------------------------------|:---------------------------------------|
|\\(a\\)                          |Plate length (long dimension)                                                         |\\({\text{m}}\\)                        |
|\\(\mathit{AR}\\)                |Aspect ratio                                                                          |--                                      |
|\\({\mathit{AR}\_{\text{max}}}\\)|Maximum aspect ratio                                                                  |--                                      |
|\\(B\\)                          |Risk of failure                                                                       |--                                      |
|\\(b\\)                          |Plate width (short dimension)                                                         |\\({\text{m}}\\)                        |
|\\(\mathit{capacity}\\)          |Capacity or load resistance                                                           |\\({\text{Pa}}\\)                       |
|\\({d\_{\text{max}}}\\)          |Maximum value for one of the dimensions of the glass plate                            |\\({\text{m}}\\)                        |
|\\({d\_{\text{min}}}\\)          |Minimum value for one of the dimensions of the glass plate                            |\\({\text{m}}\\)                        |
|\\(E\\)                          |Modulus of elasticity of glass                                                        |\\({\text{Pa}}\\)                       |
|\\(g\\)                          |Glass type                                                                            |--                                      |
|\\(\mathit{GTF}\\)               |Glass type factor                                                                     |--                                      |
|\\(h\\)                          |Minimum thickness                                                                     |\\({\text{m}}\\)                        |
|\\(\mathit{interpY}\\)           |InterpY                                                                               |--                                      |
|\\(\mathit{interpZ}\\)           |InterpZ                                                                               |--                                      |
|\\(\mathit{isSafeLoad}\\)        |Load resistance safety requirement                                                    |--                                      |
|\\(\mathit{isSafeLR}\\)          |3 second load equivalent resistance safety requirement                                |--                                      |
|\\(\mathit{isSafePb}\\)          |Probability of glass breakage safety requirement                                      |--                                      |
|\\(\mathit{isSafeProb}\\)        |Probability of failure safety requirement                                             |--                                      |
|\\(J\\)                          |Stress distribution factor (Function)                                                 |--                                      |
|\\({J\_{\text{max}}}\\)          |Maximum value for the stress distribution factor                                      |--                                      |
|\\({J\_{\text{min}}}\\)          |Minimum value for the stress distribution factor                                      |--                                      |
|\\({J\_{\text{tol}}}\\)          |Tolerable stress distribution factor                                                  |--                                      |
|\\(k\\)                          |Surface flaw parameter                                                                |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{LDF}\\)               |Load duration factor                                                                  |--                                      |
|\\(\mathit{Load}\\)              |Applied load (demand) or pressure                                                     |\\({\text{Pa}}\\)                       |
|\\(\mathit{LR}\\)                |Load resistance                                                                       |\\({\text{Pa}}\\)                       |
|\\(\mathit{LSF}\\)               |Load share factor                                                                     |--                                      |
|\\(m\\)                          |Surface flaw parameter                                                                |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{NFL}\\)               |Non-factored load                                                                     |\\({\text{Pa}}\\)                       |
|\\({P\_{\text{b}}}\\)            |Probability of breakage                                                               |--                                      |
|\\({P\_{\text{b}\text{tol}}}\\)  |Tolerable probability of breakage                                                     |--                                      |
|\\({P\_{\text{f}}}\\)            |Probability of failure                                                                |--                                      |
|\\({P\_{\text{f}\text{tol}}}\\)  |Tolerable probability of failure                                                      |--                                      |
|\\(q\\)                          |Applied load (demand)                                                                 |\\({\text{Pa}}\\)                       |
|\\(\hat{q}\\)                    |Dimensionless load                                                                    |--                                      |
|\\({\hat{q}\_{\text{tol}}}\\)    |Tolerable load                                                                        |--                                      |
|\\(\mathit{SD}\\)                |Stand off distance                                                                    |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{max}}}\\)|Maximum stand off distance permissible for input                                      |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{min}}}\\)|Minimum stand off distance permissible for input                                      |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{x}}}\\)  |Stand off distance (\\(x\\)-component)                                                |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{y}}}\\)  |Stand off distance (\\(y\\)-component)                                                |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{z}}}\\)  |Stand off distance (\\(z\\)-component)                                                |\\({\text{m}}\\)                        |
|\\(t\\)                          |Nominal thickness \\(t\in{}\{2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0\}\\)|\\({\text{mm}}\\)                       |
|\\({t\_{\text{d}}}\\)            |Duration of load                                                                      |\\({\text{s}}\\)                        |
|\\(\mathit{TNT}\\)               |TNT equivalent factor                                                                 |--                                      |
|\\(w\\)                          |Charge weight                                                                         |\\({\text{kg}}\\)                       |
|\\({w\_{\text{max}}}\\)          |Maximum permissible input charge weight                                               |\\({\text{kg}}\\)                       |
|\\({w\_{\text{min}}}\\)          |Minimum permissible input charge weight                                               |\\({\text{kg}}\\)                       |
|\\({w\_{\mathit{TNT}}}\\)        |Equivalent TNT charge mass                                                            |\\({\text{kg}}\\)                       |

**<p align="center">Table of Symbols</p>**

# Abbreviations and Acronyms {#Sec:TAbbAcc}

<div id="Table:TAbbAcc"></div>

|Abbreviation|Full Form                          |
|:-----------|:----------------------------------|
|A           |Assumption                         |
|AN          |Annealed                           |
|AR          |Aspect Ratio                       |
|DD          |Data Definition                    |
|FT          |Fully Tempered                     |
|GS          |Goal Statement                     |
|GTF         |Glass Type Factor                  |
|HS          |Heat Strengthened                  |
|IG          |Insulating Glass                   |
|IM          |Instance Model                     |
|LC          |Likely Change                      |
|LDF         |Load Duration Factor               |
|LG          |Laminated Glass                    |
|LR          |Load Resistance                    |
|LSF         |Load Share Factor                  |
|N/A         |Not Applicable                     |
|NFL         |Non-Factored Load                  |
|PS          |Physical System Description        |
|R           |Requirement                        |
|RefBy       |Referenced by                      |
|Refname     |Reference Name                     |
|SD          |Stand Off Distance                 |
|SRS         |Software Requirements Specification|
|TM          |Theoretical Model                  |
|UC          |Unlikely Change                    |
|Uncert.     |Typical Uncertainty                |

**<p align="center">Abbreviations and Acronyms</p>**

# Introduction {#Sec:Intro}

Explosions in downtown areas are dangerous from the blast itself and also potentially from the secondary effect of falling glass. Therefore, software is needed to predict whether a glass slab can withstand a blast under given conditions. For example, we might wish to know whether a pane of glass fails from a gas main explosion or from a small fertilizer truck bomb. The program documented here is called GlassBR.

The following section provides an overview of the Software Requirements Specification (SRS) for GlassBR. This section explains the purpose of this document, the scope of the requirements, the characteristics of the intended reader, and the organization of the document.

# Purpose of Document {#Sec:DocPurpose}

The primary purpose of this document is to record the requirements of GlassBR. Goals, assumptions, theoretical models, definitions, and other model derivation information are specified, allowing the reader to fully understand and verify the purpose and scientific basis of GlassBR. With the exception of [system constraints](#Sec:SysConstraints), this SRS will remain abstract, describing what problem is being solved, but not how to solve it.

This document will be used as a starting point for subsequent development phases, including writing the design specification and the software verification and validation plan. The design document will show how the requirements are to be realized, including decisions on the numerical algorithms and programming environment. The verification and validation plan will show the steps that will be used to increase confidence in the software documentation and the implementation. Although the SRS fits in a series of documents that follow the so-called waterfall model, the actual development process is not constrained in any way. Even when the waterfall model is not followed, as Parnas and Clements point out [parnasClements1986](#parnasClements1986), the most logical way to present the documentation is still to "fake" a rational design process.

# Scope of Requirements {#Sec:ReqsScope}

The scope of the requirements includes determining the safety of a glass slab under a blast loading following the ASTM standard ([astm2009](#astm2009)).

# Characteristics of Intended Reader {#Sec:ReaderChars}

Reviewers of this documentation should have an understanding of second year calculus, structural mechanics, glass breakage, blast risk, computer applications in civil engineering, and applicable standards for constructions using glass from [astm2009](#astm2009), [astm2012](#astm2012), and [astm2016](#astm2016) in [references](#Sec:References). The users of GlassBR can have a lower level of expertise, as explained in [Sec:User Characteristics](#Sec:UserChars).

# Organization of Document {#Sec:DocOrg}

The organization of this document follows the template for an SRS for scientific computing software proposed by [koothoor2013](#koothoor2013), [smithLai2005](#smithLai2005), [smithEtAl2007](#smithEtAl2007), and [smithKoothoor2016](#smithKoothoor2016). The presentation follows the standard pattern of presenting goals, theories, definitions, and assumptions. For readers that would like a more bottom up approach, they can start reading the [data definitions](#Sec:IMs) and trace back to find any additional information they require.

The [goal statements](#Sec:GoalStmt) are refined to the theoretical models and the [theoretical models](#Sec:TMs) to the [instance models](#Sec:IMs). The data definitions are used to support the definitions of the different models.

# Stakeholders {#Sec:Stakeholder}

This section describes the stakeholders: the people who have an interest in the product.

# The Client {#Sec:Client}

The client for GlassBR is a company named Entuitive. It is developed by Dr. Manuel Campidelli. The client has the final say on acceptance of the product.

# The Customer {#Sec:Customer}

The customers are the end user of GlassBR.

# General System Description {#Sec:GenSysDesc}

This section provides general information about the system. It identifies the interfaces between the system and its environment, describes the user characteristics, and lists the system constraints.

# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](#Figure:sysCtxDiag) shows the system context. A circle represents an external entity outside the software, the user in this case. A rectangle represents the software system itself (GlassBR). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag"></div>

![System Context](/assets/SystemContextFigure.png)

**<p align="center">System Context</p>**

The interaction between the product and the user is through a user interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide the input data related to the glass slab and blast type, ensuring no errors in the data entry.
  - Ensure that consistent units are used for input variables.
  - Ensure required [software assumptions](#Sec:Assumps) are appropriate for any particular problem input to the software.
- GlassBR Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number.
  - Determine if the inputs satisfy the required physical and software constraints.
  - Predict whether the glass slab is safe or not.

# User Characteristics {#Sec:UserChars}

- The end user of GlassBR is expected to have completed at least the equivalent of the second year of an undergraduate degree in civil engineering or structural engineering.
- The end user is expected to have an understanding of theory behind glass breakage and blast risk.
- The end user is expected to have basic computer literacy to handle the software.

# System Constraints {#Sec:SysConstraints}

There are no system constraints.

# Specific System Description {#Sec:SpecSystDesc}

This section first presents the problem description, which gives a high-level view of the problem to be solved. This is followed by the solution characteristics specification, which presents the assumptions, theories, and definitions that are used.

# Problem Description {#Sec:ProbDesc}

A system is needed to predict whether a glass slab can withstand a blast under given conditions.

# Terminology and Definitions {#Sec:TermDefs}

This subsection provides a list of terms that are used in the subsequent sections and their meaning, with the purpose of reducing ambiguity and making it easier to correctly understand the requirements. All of the terms are extracted from [astm2009](#astm2009).

1. Glass breakage - The fracture or breakage of any lite or ply in monolithic, laminated, or insulating glass.
2. Lateral - Perpendicular to the glass surface.
3. Lite - Pieces of glass that are cut, prepared, and used to create the window or door.
4. Specifying authority - The design professional responsible for interpreting applicable regulations of authorities having jurisdiction and considering appropriate site specific factors to determine the appropriate values used to calculate the specified design load, and furnishing other information required to perform this practice.
5. Blast resistant glazing - Glazing that provides protection against air blast pressure generated by explosions.
6. Equivalent TNT charge mass - Mass of TNT placed on the ground in a hemisphere that represents the design explosive threat.
7. Glass Type:
   - Annealed (AN) - A flat, monolithic, glass lite which has uniform thickness where the residual surface stresses are almost zero, as defined in [astm2016](#astm2016).
   - Fully tempered (FT) - A flat, monolithic, glass lite of uniform thickness that has been subjected to a special heat treatment process where the residual surface compression is not less than 69 MPa (10 000 psi) or the edge compression not less than 67 MPa (9700 psi), as defined in [astm2012](#astm2012).
   - Heat strengthened (HS) - A flat, monolithic, glass lite of uniform thickness that has been subjected to a special heat treatment process where the residual surface compression is not less than 24 MPa (3500psi) or greater than 52 MPa (7500 psi), as defined in [astm2012](#astm2012).
8. Applied load (demand) or pressure - A uniformly distributed lateral pressure.
   - Load resistance (LR) - The uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in [astm2009](#astm2009) (pp. 1 and 53).
   - Non-factored load (NFL) - Three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass.
   - Glass weight load - The dead load component of the glass weight.
   - Short duration load - Any load lasting 3 seconds or less.
   - Specified design load - The magnitude in Pa (psf), type (for example, wind or snow) and duration of the load given by the specifying authority.
   - Long duration load - Any load lasting approximately 30 days.
9. Stand off distance (SD) - The distance from the glazing surface to the centroid of a hemispherical high explosive charge. It is represented by the coordinates (\\({\mathit{SD}\_{\text{x}}}\\), \\({\mathit{SD}\_{\text{y}}}\\), \\({\mathit{SD}\_{\text{z}}}\\)).
10. Load share factor (LSF) - A multiplying factor derived from the load sharing between the double glazing, of equal or different thicknesses and types (including the layered behaviour of LG under long duration loads), in a sealed IG unit.
11. Glass type factor (GTF) - A multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions.
12. Aspect ratio (AR) - The ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5.
13. Probability of breakage (\\({P\_{\text{b}}}\\)) - The fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 ([astm2016](#astm2016)).

# Physical System Description {#Sec:PhysSyst}

The physical system of GlassBR, as shown in [Fig:physSystImage](#Figure:physSystImage), includes the following elements:

PS1: The glass slab.

PS2: The point of explosion. Where the bomb, or any kind of man-made explosion, is located. The stand off distance is the distance between the point of explosion and the glass.

<div id="Figure:physSystImage"></div>

![The physical system](/assets/physicalsystimage.png)

**<p align="center">The physical system</p>**

# Goal Statements {#Sec:GoalStmt}

Given the dimensions of the glass plane, the glass type, the characteristics of the explosion, and the tolerable probability of breakage, the goal statement is:

<div id="willBreakGS"></div>

Predict-Glass-Withstands-Explosion: Analyze and predict whether the glass slab under consideration will be able to withstand the explosion of a certain degree which is calculated based on user input.

# Solution Characteristics Specification {#Sec:SolCharSpec}

The instance models that govern GlassBR are presented in the [Instance Model Section](#Sec:IMs). The information to understand the meaning of the instance models and their derivation is also presented, so that the instance models can be verified.

# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="assumpGT"></div>

glassType: The standard E1300-09a for calculation applies only to monolithic, laminated, or insulating glass constructions of rectangular shape with continuous lateral support along one, two, three, or four edges. This practice assumes that: (1) the supported glass edges for two, three and four-sided support conditions are simply supported and free to slip in plane; (2) glass supported on two sides acts as a simply supported beam; and (3) glass supported on one side acts as a cantilever.

<div id="assumpGC"></div>

glassCondition: Following [astm2009](#astm2009) (pg. 1), this practice does not apply to any form of wired, patterned, etched, sandblasted, drilled, notched, or grooved glass with surface and edge treatments that alter the glass strength. (RefBy: [UC:Accommodate-Altered-Glass](#accAlteredGlass).)

<div id="assumpES"></div>

explainScenario: This system only considers the external explosion scenario for its calculations. (RefBy: [LC:Calculate-Internal-Blast-Risk](#calcInternalBlastRisk).)

<div id="assumpSV"></div>

standardValues: The values provided in [Sec:Values of Auxiliary Constants](#Sec:AuxConstants) are assumed for the duration of load (\\({t\_{\text{d}}}\\)), and the material properties of \\(m\\), \\(k\\), and \\(E\\). (RefBy: [IM:sdfTol](#IM:sdfTol), [IM:nFL](#IM:nFL), [IM:dimlessLoad](#IM:dimlessLoad), [LC:Variable-Values-of-m,k,E](#varValsOfmkE), [DD:loadDurFactor](#DD:loadDurFactor), and [A:ldfConstant](#assumpLDFC).)

<div id="assumpGL"></div>

glassLite: Glass under consideration is assumed to be a single lite; hence, the value of LSF is equal to 1 for all calculations in GlassBR. (RefBy: [LC:Accomodate-More-than-Single-Lite](#accMoreThanSingleLite).)

<div id="assumpBC"></div>

boundaryConditions: Boundary conditions for the glass slab are assumed to be 4-sided support for calculations. (RefBy: [LC:Accomodate-More-Boundary-Conditions](#accMoreBoundaryConditions).)

<div id="assumpRT"></div>

responseType: The response type considered in GlassBR is flexural. (RefBy: [LC:Consider-More-than-Flexure-Glass](#considerMoreThanFlexGlass).)

<div id="assumpLDFC"></div>

ldfConstant: With reference to [A:standardValues](#assumpSV), the value of load duration factor (\\(\mathit{LDF}\\)) is a constant in GlassBR. (RefBy: [LC:Variable-Values-of-m,k,E](#varValsOfmkE) and [DD:loadDurFactor](#DD:loadDurFactor).)

# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that GlassBR is based on.

<div align="center">

## Safety Probability {#TM:isSafeProb}

</div>

|Refname    |TM:isSafeProb                                                                                                                                                                                                                                                            |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Safety Probability                                                                                                                                                                                                                                                       |
|Equation   |\\[\mathit{isSafeProb}={P\_{\text{f}}}\lt{}{P\_{\text{f}\text{tol}}}\\]                                                                                                                                                                                                  |
|Description|<ul><li>\\(\mathit{isSafeProb}\\) is the probability of failure safety requirement (Unitless)</li><li>\\({P\_{\text{f}}}\\) is the probability of failure (Unitless)</li><li>\\({P\_{\text{f}\text{tol}}}\\) is the tolerable probability of failure (Unitless)</li></ul>|
|Notes      |<ul><li>If \\(\mathit{isSafeProb}\\), the structure is considered safe.</li></ul>                                                                                                                                                                                        |
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                                                         |

<div align="center">

## Safety Load {#TM:isSafeLoad}

</div>

|Refname    |TM:isSafeLoad                                                                                                                                                                                                                                                                   |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Safety Load                                                                                                                                                                                                                                                                     |
|Equation   |\\[\mathit{isSafeLoad}=\mathit{capacity}\gt{}\mathit{Load}\\]                                                                                                                                                                                                                   |
|Description|<ul><li>\\(\mathit{isSafeLoad}\\) is the load resistance safety requirement (Unitless)</li><li>\\(\mathit{capacity}\\) is the capacity or load resistance (\\({\text{Pa}}\\))</li><li>\\(\mathit{Load}\\) is the applied load (demand) or pressure (\\({\text{Pa}}\\))</li></ul>|
|Notes      |<ul><li>If \\(\mathit{isSafeLoad}\\), the structure is considered safe.</li></ul>                                                                                                                                                                                               |
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                                                |

# General Definitions {#Sec:GDs}

There are no general definitions.

# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Minimum thickness {#DD:minThick}

</div>

|Refname    |DD:minThick                                                                                                                                                                                                                                                     |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Minimum thickness                                                                                                                                                                                                                                               |
|Symbol     |\\(h\\)                                                                                                                                                                                                                                                         |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                |
|Equation   |\\[h=\frac{1}{1000} \begin{cases}2.16, & t=2.5\\\\2.59, & t=2.7\\\\2.92, & t=3.0\\\\3.78, & t=4.0\\\\4.57, & t=5.0\\\\5.56, & t=6.0\\\\7.42, & t=8.0\\\\9.02, & t=10.0\\\\11.91, & t=12.0\\\\15.09, & t=16.0\\\\18.26, & t=19.0\\\\21.44, & t=22.0\end{cases}\\]|
|Description|<ul><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(t\\) is the nominal thickness \\(t\in{}\{2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0\}\\) (\\({\text{mm}}\\))</li></ul>                                                         |
|Notes      |<ul><li>\\(t\\) is a function that maps from the nominal thickness (\\(h\\)) to the minimum thickness.</li></ul>                                                                                                                                                |
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                                           |
|RefBy      |[IM:sdfTol](#IM:sdfTol), [IM:riskFun](#IM:riskFun), [IM:nFL](#IM:nFL), and [IM:dimlessLoad](#IM:dimlessLoad)                                                                                                                                                    |

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
|Notes      |<ul><li>\\({t\_{\text{d}}}\\) and \\(m\\) come from [A:standardValues](#assumpSV).</li><li>\\(\mathit{LDF}\\) is assumed to be constant (from [A:ldfConstant](#assumpLDFC)).</li></ul>                                                       |
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                        |
|RefBy      |[IM:sdfTol](#IM:sdfTol) and [IM:riskFun](#IM:riskFun)                                                                                                                                                                                        |

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
|Source     |[astm2009](#astm2009)                                                                                                               |
|RefBy      |[IM:calofCapacity](#IM:calofCapacity) and [IM:dimlessLoad](#IM:dimlessLoad)                                                         |

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
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[DD:calofDemand](#DD:calofDemand)                                                                                                                                                                                                                                                                                                                                                                          |

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
|Source     |[astm2009](#astm2009)                                                                                                                                                                                             |
|RefBy      |[IM:tolLoad](#IM:tolLoad) and [IM:stressDistFac](#IM:stressDistFac)                                                                                                                                               |

<div align="center">

## Equivalent TNT charge mass {#DD:eqTNTW}

</div>

|Refname    |DD:eqTNTW                                                                                                                                                                                                                    |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Equivalent TNT charge mass                                                                                                                                                                                                   |
|Symbol     |\\({w\_{\mathit{TNT}}}\\)                                                                                                                                                                                                    |
|Units      |\\({\text{kg}}\\)                                                                                                                                                                                                            |
|Equation   |\\[{w\_{\mathit{TNT}}}=w \mathit{TNT}\\]                                                                                                                                                                                     |
|Description|<ul><li>\\({w\_{\mathit{TNT}}}\\) is the equivalent TNT charge mass (\\({\text{kg}}\\))</li><li>\\(w\\) is the charge weight (\\({\text{kg}}\\))</li><li>\\(\mathit{TNT}\\) is the TNT equivalent factor (Unitless)</li></ul>|
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                        |
|RefBy      |[DD:calofDemand](#DD:calofDemand)                                                                                                                                                                                            |

<div align="center">

## Applied load (demand) {#DD:calofDemand}

</div>

|Refname    |DD:calofDemand                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Applied load (demand)                                                                                                                                                                                                                                                                                                                                                                                                            |
|Symbol     |\\(q\\)                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{Pa}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                |
|Equation   |\\[q=\mathit{interpY}\left(\text{\\(\``\\)TSD.txt''},\mathit{SD},{w\_{\mathit{TNT}}}\right)\\]                                                                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\(q\\) is the applied load (demand) (\\({\text{Pa}}\\))</li><li>\\(\mathit{interpY}\\) is the interpY (Unitless)</li><li>\\(\mathit{SD}\\) is the stand off distance (\\({\text{m}}\\))</li><li>\\({w\_{\mathit{TNT}}}\\) is the equivalent TNT charge mass (\\({\text{kg}}\\))</li></ul>                                                                                                                               |
|Notes      |<ul><li>\\(q\\), or applied load (demand), is the 3 second duration equivalent pressure obtained from [Fig:demandVSsod](#Figure:demandVSsod) by interpolation using stand off distance (\\(\mathit{SD}\\)) and \\({w\_{\mathit{TNT}}}\\) as parameters. \\({w\_{\mathit{TNT}}}\\) is defined in [DD:eqTNTW](#DD:eqTNTW). \\(\mathit{SD}\\) is the stand off distance as defined in [DD:standOffDist](#DD:standOffDist).</li></ul>|
|Source     |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                                                                                            |
|RefBy      |[IM:isSafeLR](#IM:isSafeLR) and [IM:dimlessLoad](#IM:dimlessLoad)                                                                                                                                                                                                                                                                                                                                                                |

# Instance Models {#Sec:IMs}

This section transforms the problem defined in the [problem description](#Sec:ProbDesc) into one which is expressed in mathematical terms. It uses concrete symbols defined in the [data definitions](#Sec:DDs) to replace the abstract symbols in the models identified in [theoretical models](#Sec:TMs) and [general definitions](#Sec:GDs).

The goal [GS:Predict-Glass-Withstands-Explosion](#willBreakGS) is met by [IM:isSafePb](#IM:isSafePb), [IM:isSafeLR](#IM:isSafeLR).

<div align="center">

## Risk of failure {#IM:riskFun}

</div>

|Refname           |IM:riskFun                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Risk of failure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Input             |\\(E\\), \\(\mathit{LDF}\\), \\(J\\), \\(k\\), \\(m\\), \\(h\\), \\(a\\), \\(b\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Output            |\\(B\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Input Constraints |\\[a\gt{}0\\]\\[0\lt{}b\leq{}a\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Equation          |\\[B=\frac{k}{\left(a b\right)^{m-1}} \left(E h^{2}\right)^{m} \mathit{LDF} e^{J}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Description       |<ul><li>\\(B\\) is the risk of failure (Unitless)</li><li>\\(k\\) is the surface flaw parameter (\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\))</li><li>\\(a\\) is the plate length (long dimension) (\\({\text{m}}\\))</li><li>\\(b\\) is the plate width (short dimension) (\\({\text{m}}\\))</li><li>\\(m\\) is the surface flaw parameter (\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\))</li><li>\\(E\\) is the modulus of elasticity of glass (\\({\text{Pa}}\\))</li><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(\mathit{LDF}\\) is the load duration factor (Unitless)</li><li>\\(J\\) is the stress distribution factor (Function) (Unitless)</li></ul>|
|Notes             |<ul><li>\\(a\\) and \\(b\\) are the dimensions of the plate, where (\\(a\geq{}b\\)).</li><li>\\(h\\) is defined in [DD:minThick](#DD:minThick) and is based on the nominal thicknesses.</li><li>\\(\mathit{LDF}\\) is defined in [DD:loadDurFactor](#DD:loadDurFactor).</li><li>\\(J\\) is defined in [IM:stressDistFac](#IM:stressDistFac).</li></ul>                                                                                                                                                                                                                                                                                                                       |
|Source            |[astm2009](#astm2009), [beasonEtAl1998](#beasonEtAl1998) (Eqs. 4-5), and [campidelli](#campidelli) (Eq. 14)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|RefBy             |[IM:probOfBreak](#IM:probOfBreak)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

<div align="center">

## Stress distribution factor (Function) {#IM:stressDistFac}

</div>

|Refname           |IM:stressDistFac                                                                                                                                                                                                                                                                   |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Stress distribution factor (Function)                                                                                                                                                                                                                                              |
|Input             |\\(\mathit{AR}\\), \\(\hat{q}\\)                                                                                                                                                                                                                                                   |
|Output            |\\(J\\)                                                                                                                                                                                                                                                                            |
|Input Constraints |\\[\mathit{AR}\geq{}1\\]                                                                                                                                                                                                                                                           |
|Output Constraints|\\[{J\_{\text{min}}}\leq{}J\leq{}{J\_{\text{max}}}\\]                                                                                                                                                                                                                              |
|Equation          |\\[J=\mathit{interpZ}\left(\text{\\(\``\\)SDF.txt''},\mathit{AR},\hat{q}\right)\\]                                                                                                                                                                                                 |
|Description       |<ul><li>\\(J\\) is the stress distribution factor (Function) (Unitless)</li><li>\\(\mathit{interpZ}\\) is the interpZ (Unitless)</li><li>\\(\mathit{AR}\\) is the aspect ratio (Unitless)</li><li>\\(\hat{q}\\) is the dimensionless load (Unitless)</li></ul>                     |
|Notes             |<ul><li>\\(J\\) is obtained by interpolating from data shown in [Fig:dimlessloadVSaspect](#Figure:dimlessloadVSaspect).</li><li>\\(\mathit{AR}\\) is defined in [DD:aspectRatio](#DD:aspectRatio).</li><li>\\(\hat{q}\\) is defined in [IM:dimlessLoad](#IM:dimlessLoad).</li></ul>|
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                              |
|RefBy             |[IM:riskFun](#IM:riskFun)                                                                                                                                                                                                                                                          |

<div align="center">

## Non-factored load {#IM:nFL}

</div>

|Refname           |IM:nFL                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|:-----------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Non-factored load                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Input             |\\({\hat{q}\_{\text{tol}}}\\), \\(E\\), \\(h\\), \\(a\\), \\(b\\)                                                                                                                                                                                                                                                                                                                                                                             |
|Output            |\\(\mathit{NFL}\\)                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Input Constraints |\\[a\gt{}0\\]\\[0\lt{}b\leq{}a\\]                                                                                                                                                                                                                                                                                                                                                                                                             |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Equation          |\\[\mathit{NFL}=\frac{{\hat{q}\_{\text{tol}}} E h^{4}}{\left(a b\right)^{2}}\\]                                                                                                                                                                                                                                                                                                                                                               |
|Description       |<ul><li>\\(\mathit{NFL}\\) is the non-factored load (\\({\text{Pa}}\\))</li><li>\\({\hat{q}\_{\text{tol}}}\\) is the tolerable load (Unitless)</li><li>\\(E\\) is the modulus of elasticity of glass (\\({\text{Pa}}\\))</li><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(a\\) is the plate length (long dimension) (\\({\text{m}}\\))</li><li>\\(b\\) is the plate width (short dimension) (\\({\text{m}}\\))</li></ul>|
|Notes             |<ul><li>\\({\hat{q}\_{\text{tol}}}\\) is defined in [IM:tolLoad](#IM:tolLoad).</li><li>\\(E\\) comes from [A:standardValues](#assumpSV).</li><li>\\(h\\) is defined in [DD:minThick](#DD:minThick) and is based on the nominal thicknesses.</li><li>\\(a\\) and \\(b\\) are the dimensions of the plate, where (\\(a\geq{}b\\)).</li></ul>                                                                                                    |
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                                                                                                         |
|RefBy             |[IM:calofCapacity](#IM:calofCapacity)                                                                                                                                                                                                                                                                                                                                                                                                         |

<div align="center">

## Dimensionless load {#IM:dimlessLoad}

</div>

|Refname           |IM:dimlessLoad                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:-----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Dimensionless load                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Input             |\\(q\\), \\(E\\), \\(h\\), \\(\mathit{GTF}\\), \\(a\\), \\(b\\)                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Output            |\\(\hat{q}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Input Constraints |\\[a\gt{}0\\]\\[0\lt{}b\leq{}a\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Equation          |\\[\hat{q}=\frac{q \left(a b\right)^{2}}{E h^{4} \mathit{GTF}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Description       |<ul><li>\\(\hat{q}\\) is the dimensionless load (Unitless)</li><li>\\(q\\) is the applied load (demand) (\\({\text{Pa}}\\))</li><li>\\(a\\) is the plate length (long dimension) (\\({\text{m}}\\))</li><li>\\(b\\) is the plate width (short dimension) (\\({\text{m}}\\))</li><li>\\(E\\) is the modulus of elasticity of glass (\\({\text{Pa}}\\))</li><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(\mathit{GTF}\\) is the glass type factor (Unitless)</li></ul>|
|Notes             |<ul><li>\\(q\\) is the 3 second duration equivalent pressure, as given in [DD:calofDemand](#DD:calofDemand).</li><li>\\(a\\) and \\(b\\) are the dimensions of the plate, where (\\(a\geq{}b\\)).</li><li>\\(E\\) comes from [A:standardValues](#assumpSV).</li><li>\\(h\\) is defined in [DD:minThick](#DD:minThick) and is based on the nominal thicknesses.</li><li>\\(\mathit{GTF}\\) is defined in [DD:gTF](#DD:gTF).</li></ul>                                                      |
|Source            |[astm2009](#astm2009) and [campidelli](#campidelli) (Eq. 7)                                                                                                                                                                                                                                                                                                                                                                                                                               |
|RefBy             |[IM:stressDistFac](#IM:stressDistFac)                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

<div align="center">

## Tolerable load {#IM:tolLoad}

</div>

|Refname           |IM:tolLoad                                                                                                                                                                                                                                                                                               |
|:-----------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Tolerable load                                                                                                                                                                                                                                                                                           |
|Input             |\\(\mathit{AR}\\), \\({J\_{\text{tol}}}\\)                                                                                                                                                                                                                                                               |
|Output            |\\({\hat{q}\_{\text{tol}}}\\)                                                                                                                                                                                                                                                                            |
|Input Constraints |\\[\mathit{AR}\geq{}1\\]                                                                                                                                                                                                                                                                                 |
|Output Constraints|                                                                                                                                                                                                                                                                                                         |
|Equation          |\\[{\hat{q}\_{\text{tol}}}=\mathit{interpY}\left(\text{\\(\``\\)SDF.txt''},\mathit{AR},{J\_{\text{tol}}}\right)\\]                                                                                                                                                                                       |
|Description       |<ul><li>\\({\hat{q}\_{\text{tol}}}\\) is the tolerable load (Unitless)</li><li>\\(\mathit{interpY}\\) is the interpY (Unitless)</li><li>\\(\mathit{AR}\\) is the aspect ratio (Unitless)</li><li>\\({J\_{\text{tol}}}\\) is the tolerable stress distribution factor (Unitless)</li></ul>                |
|Notes             |<ul><li>\\({\hat{q}\_{\text{tol}}}\\) is obtained by interpolating from data shown in [Fig:dimlessloadVSaspect](#Figure:dimlessloadVSaspect).</li><li>\\(\mathit{AR}\\) is defined in [DD:aspectRatio](#DD:aspectRatio).</li><li>\\({J\_{\text{tol}}}\\) is defined in [IM:sdfTol](#IM:sdfTol).</li></ul>|
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                    |
|RefBy             |[IM:nFL](#IM:nFL)                                                                                                                                                                                                                                                                                        |

<div align="center">

## Tolerable stress distribution factor {#IM:sdfTol}

</div>

|Refname           |IM:sdfTol                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:-----------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Tolerable stress distribution factor                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Input             |\\(\mathit{LDF}\\), \\({P\_{\text{b}\text{tol}}}\\), \\(E\\), \\(a\\), \\(b\\), \\(m\\), \\(k\\), \\(h\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Output            |\\({J\_{\text{tol}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Input Constraints |\\[0\leq{}{P\_{\text{b}\text{tol}}}\leq{}1\\]\\[a\gt{}0\\]\\[0\lt{}b\leq{}a\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Equation          |\\[{J\_{\text{tol}}}=\ln\left(\ln\left(\frac{1}{1-{P\_{\text{b}\text{tol}}}}\right) \frac{\left(a b\right)^{m-1}}{k \left(E h^{2}\right)^{m} \mathit{LDF}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Description       |<ul><li>\\({J\_{\text{tol}}}\\) is the tolerable stress distribution factor (Unitless)</li><li>\\({P\_{\text{b}\text{tol}}}\\) is the tolerable probability of breakage (Unitless)</li><li>\\(a\\) is the plate length (long dimension) (\\({\text{m}}\\))</li><li>\\(b\\) is the plate width (short dimension) (\\({\text{m}}\\))</li><li>\\(m\\) is the surface flaw parameter (\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\))</li><li>\\(k\\) is the surface flaw parameter (\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\))</li><li>\\(E\\) is the modulus of elasticity of glass (\\({\text{Pa}}\\))</li><li>\\(h\\) is the minimum thickness (\\({\text{m}}\\))</li><li>\\(\mathit{LDF}\\) is the load duration factor (Unitless)</li></ul>|
|Notes             |<ul><li>\\({P\_{\text{b}\text{tol}}}\\) is entered by the user.</li><li>\\(a\\) and \\(b\\) are the dimensions of the plate, where (\\(a\geq{}b\\)).</li><li>\\(m\\), \\(k\\), and \\(E\\) come from [A:standardValues](#assumpSV).</li><li>\\(h\\) is defined in [DD:minThick](#DD:minThick) and is based on the nominal thicknesses.</li><li>\\(\mathit{LDF}\\) is defined in [DD:loadDurFactor](#DD:loadDurFactor).</li></ul>                                                                                                                                                                                                                                                                                                      |
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy             |[IM:tolLoad](#IM:tolLoad)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |

<div align="center">

## Probability of breakage {#IM:probOfBreak}

</div>

|Refname           |IM:probOfBreak                                                                                                                     |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------|
|Label             |Probability of breakage                                                                                                            |
|Input             |\\(B\\)                                                                                                                            |
|Output            |\\({P\_{\text{b}}}\\)                                                                                                              |
|Input Constraints |                                                                                                                                   |
|Output Constraints|\\[0\leq{}{P\_{\text{b}}}\leq{}1\\]                                                                                                |
|Equation          |\\[{P\_{\text{b}}}=1-e^{-B}\\]                                                                                                     |
|Description       |<ul><li>\\({P\_{\text{b}}}\\) is the probability of breakage (Unitless)</li><li>\\(B\\) is the risk of failure (Unitless)</li></ul>|
|Notes             |<ul><li>\\(B\\) is defined in [IM:riskFun](#IM:riskFun).</li></ul>                                                                 |
|Source            |[astm2009](#astm2009) and [beasonEtAl1998](#beasonEtAl1998)                                                                        |
|RefBy             |[IM:isSafePb](#IM:isSafePb)                                                                                                        |

<div align="center">

## Load resistance {#IM:calofCapacity}

</div>

|Refname           |IM:calofCapacity                                                                                                                                                                                                                                                                    |
|:-----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Load resistance                                                                                                                                                                                                                                                                     |
|Input             |\\(\mathit{NFL}\\), \\(\mathit{GTF}\\), \\(\mathit{LSF}\\)                                                                                                                                                                                                                          |
|Output            |\\(\mathit{LR}\\)                                                                                                                                                                                                                                                                   |
|Input Constraints |                                                                                                                                                                                                                                                                                    |
|Output Constraints|                                                                                                                                                                                                                                                                                    |
|Equation          |\\[\mathit{LR}=\mathit{NFL} \mathit{GTF} \mathit{LSF}\\]                                                                                                                                                                                                                            |
|Description       |<ul><li>\\(\mathit{LR}\\) is the load resistance (\\({\text{Pa}}\\))</li><li>\\(\mathit{NFL}\\) is the non-factored load (\\({\text{Pa}}\\))</li><li>\\(\mathit{GTF}\\) is the glass type factor (Unitless)</li><li>\\(\mathit{LSF}\\) is the load share factor (Unitless)</li></ul>|
|Notes             |<ul><li>\\(\mathit{LR}\\) is also called capacity.</li><li>\\(\mathit{NFL}\\) is defined in [IM:nFL](#IM:nFL).</li><li>\\(\mathit{GTF}\\) is defined in [DD:gTF](#DD:gTF).</li></ul>                                                                                                |
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                               |
|RefBy             |[IM:isSafeLR](#IM:isSafeLR)                                                                                                                                                                                                                                                         |

<div align="center">

## Safety Req-Pb {#IM:isSafePb}

</div>

|Refname           |IM:isSafePb                                                                                                                                                                                                                                                                                                                                          |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Safety Req-Pb                                                                                                                                                                                                                                                                                                                                        |
|Input             |\\({P\_{\text{b}}}\\), \\({P\_{\text{b}\text{tol}}}\\)                                                                                                                                                                                                                                                                                               |
|Output            |\\(\mathit{isSafePb}\\)                                                                                                                                                                                                                                                                                                                              |
|Input Constraints |\\[0\leq{}{P\_{\text{b}}}\leq{}1\\]\\[0\leq{}{P\_{\text{b}\text{tol}}}\leq{}1\\]                                                                                                                                                                                                                                                                     |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                     |
|Equation          |\\[\mathit{isSafePb}={P\_{\text{b}}}\lt{}{P\_{\text{b}\text{tol}}}\\]                                                                                                                                                                                                                                                                                |
|Description       |<ul><li>\\(\mathit{isSafePb}\\) is the probability of glass breakage safety requirement (Unitless)</li><li>\\({P\_{\text{b}}}\\) is the probability of breakage (Unitless)</li><li>\\({P\_{\text{b}\text{tol}}}\\) is the tolerable probability of breakage (Unitless)</li></ul>                                                                     |
|Notes             |<ul><li>If \\(\mathit{isSafePb}\\), the glass is considered safe. \\(\mathit{isSafePb}\\) and \\(\mathit{isSafeLR}\\) (from [IM:isSafeLR](#IM:isSafeLR)) are either both True or both False.</li><li>\\({P\_{\text{b}}}\\) is defined in [IM:probOfBreak](#IM:probOfBreak).</li><li>\\({P\_{\text{b}\text{tol}}}\\) is entered by the user.</li></ul>|
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                |
|RefBy             |[IM:isSafeLR](#IM:isSafeLR) and [FR:Check-Glass-Safety](#checkGlassSafety)                                                                                                                                                                                                                                                                           |

<div align="center">

## Safety Req-LR {#IM:isSafeLR}

</div>

|Refname           |IM:isSafeLR                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:-----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Safety Req-LR                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Input             |\\(\mathit{LR}\\), \\(q\\)                                                                                                                                                                                                                                                                                                                                                                                                    |
|Output            |\\(\mathit{isSafeLR}\\)                                                                                                                                                                                                                                                                                                                                                                                                       |
|Input Constraints |\\[\mathit{LR}\gt{}0\\]\\[q\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                          |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Equation          |\\[\mathit{isSafeLR}=\mathit{LR}\gt{}q\\]                                                                                                                                                                                                                                                                                                                                                                                     |
|Description       |<ul><li>\\(\mathit{isSafeLR}\\) is the 3 second load equivalent resistance safety requirement (Unitless)</li><li>\\(\mathit{LR}\\) is the load resistance (\\({\text{Pa}}\\))</li><li>\\(q\\) is the applied load (demand) (\\({\text{Pa}}\\))</li></ul>                                                                                                                                                                      |
|Notes             |<ul><li>If \\(\mathit{isSafeLR}\\), the glass is considered safe. \\(\mathit{isSafePb}\\) (from [IM:isSafePb](#IM:isSafePb)) and \\(\mathit{isSafeLR}\\) are either both True or both False.</li><li>\\(\mathit{LR}\\) is defined in [IM:calofCapacity](#IM:calofCapacity) and is also called capacity.</li><li>\\(q\\) is the 3 second duration equivalent pressure, as given in [DD:calofDemand](#DD:calofDemand).</li></ul>|
|Source            |[astm2009](#astm2009)                                                                                                                                                                                                                                                                                                                                                                                                         |
|RefBy             |[IM:isSafePb](#IM:isSafePb) and [FR:Check-Glass-Safety](#checkGlassSafety)                                                                                                                                                                                                                                                                                                                                                    |

# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario. The [auxiliary constants](#Sec:AuxConstants) give the values of the specification parameters used in the [Data Constraints Table](#Table:InDataConstraints).

<div id="Table:InDataConstraints"></div>

|Var                            |Physical Constraints                         |Software Constraints                                                               |Typical Value             |Uncert.     |
|:------------------------------|:--------------------------------------------|:----------------------------------------------------------------------------------|:-------------------------|:-----------|
|\\(a\\)                        |\\(a\gt{}0\land{}a\geq{}b\\)                 |\\({d\_{\text{min}}}\leq{}a\leq{}{d\_{\text{max}}}\\)                              |\\(1.5\\) \\({\text{m}}\\)|10\\(\\%\\) |
|\\(\mathit{AR}\\)              |\\(\mathit{AR}\geq{}1\\)                     |\\(\mathit{AR}\leq{}{\mathit{AR}\_{\text{max}}}\\)                                 |\\(1.5\\)                 |10\\(\\%\\) |
|\\(b\\)                        |\\(0\lt{}b\leq{}a\\)                         |\\({d\_{\text{min}}}\leq{}b\leq{}{d\_{\text{max}}}\\)                              |\\(1.2\\) \\({\text{m}}\\)|10\\(\\%\\) |
|\\({P\_{\text{b}\text{tol}}}\\)|\\(0\leq{}{P\_{\text{b}\text{tol}}}\leq{}1\\)|--                                                                                 |\\(0.008\\)               |0.1\\(\\%\\)|
|\\(\mathit{SD}\\)              |\\(\mathit{SD}\gt{}0\\)                      |\\({\mathit{SD}\_{\text{min}}}\leq{}\mathit{SD}\leq{}{\mathit{SD}\_{\text{max}}}\\)|\\(45\\) \\({\text{m}}\\) |10\\(\\%\\) |
|\\(\mathit{TNT}\\)             |\\(\mathit{TNT}\gt{}0\\)                     |--                                                                                 |\\(1\\)                   |10\\(\\%\\) |
|\\(w\\)                        |\\(w\gt{}0\\)                                |\\({w\_{\text{min}}}\leq{}w\leq{}{w\_{\text{max}}}\\)                              |\\(42\\) \\({\text{kg}}\\)|10\\(\\%\\) |

**<p align="center">Input Data Constraints</p>**

# Properties of a Correct Solution {#Sec:CorSolProps}

The [Data Constraints Table](#Table:OutDataConstraints) shows the data constraints on the output variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable.

<div id="Table:OutDataConstraints"></div>

|Var                  |Physical Constraints                                 |
|:--------------------|:----------------------------------------------------|
|\\({P\_{\text{b}}}\\)|\\(0\leq{}{P\_{\text{b}}}\leq{}1\\)                  |
|\\(J\\)              |\\({J\_{\text{min}}}\leq{}J\leq{}{J\_{\text{max}}}\\)|

**<p align="center">Output Data Constraints</p>**

# Requirements {#Sec:Requirements}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete, and the non-functional requirements, the qualities that the software is expected to exhibit.

# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](#Table:ReqInputs), which define the glass dimensions, type of glass, tolerable probability of failure, and the characteristics of the blast.

<div id="sysSetValsFollowingAssumps"></div>

System-Set-Values-Following-Assumptions: The system shall set the known values as described in the table for [Required Assignments](#Table:ReqAssignments).

<div id="checkInputWithDataCons"></div>

Check-Input-with-Data_Constraints: The system shall check the entered input values to ensure that they do not exceed the [data constraints](#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="outputValsAndKnownValues"></div>

Output-Values-and-Known-Values: Output the input values from [FR:Input-Values](#inputValues) and the known values from [FR:System-Set-Values-Following-Assumptions](#sysSetValsFollowingAssumps).

<div id="checkGlassSafety"></div>

Check-Glass-Safety: If \\(\mathit{isSafePb}\land{}\mathit{isSafeLR}\\) (from [IM:isSafePb](#IM:isSafePb) and [IM:isSafeLR](#IM:isSafeLR)), output the message "For the given input parameters, the glass is considered safe." If the condition is false, then output the message "For the given input parameters, the glass is NOT considered safe."

<div id="outputValues"></div>

Output-Values: Output the values from the table for [Required Outputs](#Table:ReqOutputs).

<div id="Table:ReqInputs"></div>

|Symbol                         |Description                                                                           |Units            |
|:------------------------------|:-------------------------------------------------------------------------------------|:----------------|
|\\(a\\)                        |Plate length (long dimension)                                                         |\\({\text{m}}\\) |
|\\(b\\)                        |Plate width (short dimension)                                                         |\\({\text{m}}\\) |
|\\(g\\)                        |Glass type                                                                            |--               |
|\\({P\_{\text{b}\text{tol}}}\\)|Tolerable probability of breakage                                                     |--               |
|\\({\mathit{SD}\_{\text{x}}}\\)|Stand off distance (\\(x\\)-component)                                                |\\({\text{m}}\\) |
|\\({\mathit{SD}\_{\text{y}}}\\)|Stand off distance (\\(y\\)-component)                                                |\\({\text{m}}\\) |
|\\({\mathit{SD}\_{\text{z}}}\\)|Stand off distance (\\(z\\)-component)                                                |\\({\text{m}}\\) |
|\\(t\\)                        |Nominal thickness \\(t\in{}\{2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0\}\\)|\\({\text{mm}}\\)|
|\\(\mathit{TNT}\\)             |TNT equivalent factor                                                                 |--               |
|\\(w\\)                        |Charge weight                                                                         |\\({\text{kg}}\\)|

**<p align="center">Required Inputs following [FR:Input-Values](#inputValues)</p>**

<div id="Table:ReqAssignments"></div>

|Symbol               |Description                   |Source                               |Units                                   |
|:--------------------|:-----------------------------|:------------------------------------|:---------------------------------------|
|\\(\mathit{AR}\\)    |Aspect ratio                  |[DD:aspectRatio](#DD:aspectRatio)    |--                                      |
|\\(E\\)              |Modulus of elasticity of glass|[A:standardValues](#assumpSV)        |\\({\text{Pa}}\\)                       |
|\\(\mathit{GTF}\\)   |Glass type factor             |[DD:gTF](#DD:gTF)                    |--                                      |
|\\(h\\)              |Minimum thickness             |[DD:minThick](#DD:minThick)          |\\({\text{m}}\\)                        |
|\\(k\\)              |Surface flaw parameter        |[A:standardValues](#assumpSV)        |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{LDF}\\)   |Load duration factor          |[DD:loadDurFactor](#DD:loadDurFactor)|--                                      |
|\\(\mathit{LSF}\\)   |Load share factor             |[A:glassLite](#assumpGL)             |--                                      |
|\\(m\\)              |Surface flaw parameter        |[A:standardValues](#assumpSV)        |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{SD}\\)    |Stand off distance            |[DD:standOffDist](#DD:standOffDist)  |\\({\text{m}}\\)                        |
|\\({t\_{\text{d}}}\\)|Duration of load              |[A:standardValues](#assumpSV)        |\\({\text{s}}\\)                        |

**<p align="center">Required Assignments following [FR:System-Set-Values-Following-Assumptions](#sysSetValsFollowingAssumps)</p>**

<div id="Table:ReqOutputs"></div>

|Symbol                       |Description                                           |Source                               |Units            |
|:----------------------------|:-----------------------------------------------------|:------------------------------------|:----------------|
|\\(\mathit{AR}\\)            |Aspect ratio                                          |[DD:aspectRatio](#DD:aspectRatio)    |--               |
|\\(B\\)                      |Risk of failure                                       |[IM:riskFun](#IM:riskFun)            |--               |
|\\(\mathit{GTF}\\)           |Glass type factor                                     |[DD:gTF](#DD:gTF)                    |--               |
|\\(h\\)                      |Minimum thickness                                     |[DD:minThick](#DD:minThick)          |\\({\text{m}}\\) |
|\\(\mathit{isSafeLR}\\)      |3 second load equivalent resistance safety requirement|[IM:isSafeLR](#IM:isSafeLR)          |--               |
|\\(\mathit{isSafePb}\\)      |Probability of glass breakage safety requirement      |[IM:isSafePb](#IM:isSafePb)          |--               |
|\\(J\\)                      |Stress distribution factor (Function)                 |[IM:stressDistFac](#IM:stressDistFac)|--               |
|\\({J\_{\text{tol}}}\\)      |Tolerable stress distribution factor                  |[IM:sdfTol](#IM:sdfTol)              |--               |
|\\(\mathit{LR}\\)            |Load resistance                                       |[IM:calofCapacity](#IM:calofCapacity)|\\({\text{Pa}}\\)|
|\\(\mathit{NFL}\\)           |Non-factored load                                     |[IM:nFL](#IM:nFL)                    |\\({\text{Pa}}\\)|
|\\({P\_{\text{b}}}\\)        |Probability of breakage                               |[IM:probOfBreak](#IM:probOfBreak)    |--               |
|\\(\hat{q}\\)                |Dimensionless load                                    |[IM:dimlessLoad](#IM:dimlessLoad)    |--               |
|\\({\hat{q}\_{\text{tol}}}\\)|Tolerable load                                        |[IM:tolLoad](#IM:tolLoad)            |--               |

**<p align="center">Required Outputs following [FR:Output-Values](#outputValues)</p>**

# Non-Functional Requirements {#Sec:NFRs}

This section provides the non-functional requirements, the qualities that the software is expected to exhibit.

<div id="correct"></div>

Correct: The outputs of the code have the properties described in [Sec:Properties of a Correct Solution](#Sec:CorSolProps).

<div id="verifiable"></div>

Verifiable: The code is tested with complete verification and validation plan.

<div id="understandable"></div>

Understandable: The code is modularized with complete module guide and module interface specification.

<div id="reusable"></div>

Reusable: The code is modularized.

<div id="maintainable"></div>

Maintainable: If a likely change is made to the finished software, it will take at most 10\\(\\%\\) of the original development time, assuming the same development resources are available.

<div id="portable"></div>

Portable: The code is able to be run in different environments.

# Likely Changes {#Sec:LCs}

This section lists the likely changes to be made to the software.

<div id="calcInternalBlastRisk"></div>

Calculate-Internal-Blast-Risk: [A:explainScenario](#assumpES) - The system currently only calculates for external blast risk. In the future, calculations can be added for the internal blast risk.

<div id="varValsOfmkE"></div>

Variable-Values-of-m,k,E: [A:standardValues](#assumpSV), [A:ldfConstant](#assumpLDFC) - Currently, the values for \\(m\\), \\(k\\), and \\(E\\) are assumed to be the same for all glass. In the future, these values can be changed to variable inputs.

<div id="accMoreThanSingleLite"></div>

Accomodate-More-than-Single-Lite: [A:glassLite](#assumpGL) - The software may be changed to accommodate more than a single lite.

<div id="accMoreBoundaryConditions"></div>

Accomodate-More-Boundary-Conditions: [A:boundaryConditions](#assumpBC) - The software may be changed to accommodate more boundary conditions than 4-sided support.

<div id="considerMoreThanFlexGlass"></div>

Consider-More-than-Flexure-Glass: [A:responseType](#assumpRT) - The software may be changed to consider more than just flexure of the glass.

# Unlikely Changes {#Sec:UCs}

This section lists the unlikely changes to be made to the software.

<div id="predictWithstandOfCertDeg"></div>

Predict-Withstanding-of-Certain-Degree: The goal of the system is to predict whether the glass slab under consideration can withstand an explosion of a certain degree.

<div id="accAlteredGlass"></div>

Accommodate-Altered-Glass: [A:glassCondition](#assumpGC) requires that the glass is not altered in any way. Therefore, this cannot be used on altered glass.

# Traceability Matrices and Graphs {#Sec:TraceMatrices}

The purpose of the traceability matrices is to provide easy references on what has to be additionally modified if a certain component is changed. Every time a component is changed, the items in the column of that component that are marked with an "X" should be modified as well. [Tab:TraceMatAvsA](#Table:TraceMatAvsA) shows the dependencies of the assumptions on each other. [Tab:TraceMatAvsAll](#Table:TraceMatAvsAll) shows the dependencies of the data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Tab:TraceMatRefvsRef](#Table:TraceMatRefvsRef) shows the dependencies of the data definitions, theoretical models, general definitions, and instance models on each other. [Tab:TraceMatAllvsR](#Table:TraceMatAllvsR) shows the dependencies of the requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models.

<div id="Table:TraceMatAvsA"></div>

|                                 |[A:glassType](#assumpGT)|[A:glassCondition](#assumpGC)|[A:explainScenario](#assumpES)|[A:standardValues](#assumpSV)|[A:glassLite](#assumpGL)|[A:boundaryConditions](#assumpBC)|[A:responseType](#assumpRT)|[A:ldfConstant](#assumpLDFC)|
|:--------------------------------|:-----------------------|:----------------------------|:-----------------------------|:----------------------------|:-----------------------|:--------------------------------|:--------------------------|:---------------------------|
|[A:glassType](#assumpGT)         |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:glassCondition](#assumpGC)    |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:explainScenario](#assumpES)   |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:standardValues](#assumpSV)    |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:glassLite](#assumpGL)         |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:boundaryConditions](#assumpBC)|                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:responseType](#assumpRT)      |                        |                             |                              |                             |                        |                                 |                           |                            |
|[A:ldfConstant](#assumpLDFC)     |                        |                             |                              |X                            |                        |                                 |                           |                            |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Assumptions</p>**

<div id="Table:TraceMatAvsAll"></div>

|                                                                         |[A:glassType](#assumpGT)|[A:glassCondition](#assumpGC)|[A:explainScenario](#assumpES)|[A:standardValues](#assumpSV)|[A:glassLite](#assumpGL)|[A:boundaryConditions](#assumpBC)|[A:responseType](#assumpRT)|[A:ldfConstant](#assumpLDFC)|
|:------------------------------------------------------------------------|:-----------------------|:----------------------------|:-----------------------------|:----------------------------|:-----------------------|:--------------------------------|:--------------------------|:---------------------------|
|[DD:minThick](#DD:minThick)                                              |                        |                             |                              |                             |                        |                                 |                           |                            |
|[DD:loadDurFactor](#DD:loadDurFactor)                                    |                        |                             |                              |X                            |                        |                                 |                           |X                           |
|[DD:gTF](#DD:gTF)                                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[DD:standOffDist](#DD:standOffDist)                                      |                        |                             |                              |                             |                        |                                 |                           |                            |
|[DD:aspectRatio](#DD:aspectRatio)                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[DD:eqTNTW](#DD:eqTNTW)                                                  |                        |                             |                              |                             |                        |                                 |                           |                            |
|[DD:calofDemand](#DD:calofDemand)                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[TM:isSafeProb](#TM:isSafeProb)                                          |                        |                             |                              |                             |                        |                                 |                           |                            |
|[TM:isSafeLoad](#TM:isSafeLoad)                                          |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:riskFun](#IM:riskFun)                                                |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:stressDistFac](#IM:stressDistFac)                                    |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:nFL](#IM:nFL)                                                        |                        |                             |                              |X                            |                        |                                 |                           |                            |
|[IM:dimlessLoad](#IM:dimlessLoad)                                        |                        |                             |                              |X                            |                        |                                 |                           |                            |
|[IM:tolLoad](#IM:tolLoad)                                                |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:sdfTol](#IM:sdfTol)                                                  |                        |                             |                              |X                            |                        |                                 |                           |                            |
|[IM:probOfBreak](#IM:probOfBreak)                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:calofCapacity](#IM:calofCapacity)                                    |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:isSafePb](#IM:isSafePb)                                              |                        |                             |                              |                             |                        |                                 |                           |                            |
|[IM:isSafeLR](#IM:isSafeLR)                                              |                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:Input-Values](#inputValues)                                          |                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:System-Set-Values-Following-Assumptions](#sysSetValsFollowingAssumps)|                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:Check-Input-with-Data_Constraints](#checkInputWithDataCons)          |                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:Output-Values-and-Known-Values](#outputValsAndKnownValues)           |                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:Check-Glass-Safety](#checkGlassSafety)                               |                        |                             |                              |                             |                        |                                 |                           |                            |
|[FR:Output-Values](#outputValues)                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Correct](#correct)                                                  |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Verifiable](#verifiable)                                            |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Understandable](#understandable)                                    |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Reusable](#reusable)                                                |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Maintainable](#maintainable)                                        |                        |                             |                              |                             |                        |                                 |                           |                            |
|[NFR:Portable](#portable)                                                |                        |                             |                              |                             |                        |                                 |                           |                            |
|[LC:Calculate-Internal-Blast-Risk](#calcInternalBlastRisk)               |                        |                             |X                             |                             |                        |                                 |                           |                            |
|[LC:Variable-Values-of-m,k,E](#varValsOfmkE)                             |                        |                             |                              |X                            |                        |                                 |                           |X                           |
|[LC:Accomodate-More-than-Single-Lite](#accMoreThanSingleLite)            |                        |                             |                              |                             |X                       |                                 |                           |                            |
|[LC:Accomodate-More-Boundary-Conditions](#accMoreBoundaryConditions)     |                        |                             |                              |                             |                        |X                                |                           |                            |
|[LC:Consider-More-than-Flexure-Glass](#considerMoreThanFlexGlass)        |                        |                             |                              |                             |                        |                                 |X                          |                            |
|[UC:Predict-Withstanding-of-Certain-Degree](#predictWithstandOfCertDeg)  |                        |                             |                              |                             |                        |                                 |                           |                            |
|[UC:Accommodate-Altered-Glass](#accAlteredGlass)                         |                        |X                            |                              |                             |                        |                                 |                           |                            |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Items</p>**

<div id="Table:TraceMatRefvsRef"></div>

|                                     |[DD:minThick](#DD:minThick)|[DD:loadDurFactor](#DD:loadDurFactor)|[DD:gTF](#DD:gTF)|[DD:standOffDist](#DD:standOffDist)|[DD:aspectRatio](#DD:aspectRatio)|[DD:eqTNTW](#DD:eqTNTW)|[DD:calofDemand](#DD:calofDemand)|[TM:isSafeProb](#TM:isSafeProb)|[TM:isSafeLoad](#TM:isSafeLoad)|[IM:riskFun](#IM:riskFun)|[IM:stressDistFac](#IM:stressDistFac)|[IM:nFL](#IM:nFL)|[IM:dimlessLoad](#IM:dimlessLoad)|[IM:tolLoad](#IM:tolLoad)|[IM:sdfTol](#IM:sdfTol)|[IM:probOfBreak](#IM:probOfBreak)|[IM:calofCapacity](#IM:calofCapacity)|[IM:isSafePb](#IM:isSafePb)|[IM:isSafeLR](#IM:isSafeLR)|
|:------------------------------------|:--------------------------|:------------------------------------|:----------------|:----------------------------------|:--------------------------------|:----------------------|:--------------------------------|:------------------------------|:------------------------------|:------------------------|:------------------------------------|:----------------|:--------------------------------|:------------------------|:----------------------|:--------------------------------|:------------------------------------|:--------------------------|:--------------------------|
|[DD:minThick](#DD:minThick)          |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:loadDurFactor](#DD:loadDurFactor)|                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:gTF](#DD:gTF)                    |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:standOffDist](#DD:standOffDist)  |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:aspectRatio](#DD:aspectRatio)    |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:eqTNTW](#DD:eqTNTW)              |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[DD:calofDemand](#DD:calofDemand)    |                           |                                     |                 |X                                  |                                 |X                      |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[TM:isSafeProb](#TM:isSafeProb)      |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[TM:isSafeLoad](#TM:isSafeLoad)      |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:riskFun](#IM:riskFun)            |X                          |X                                    |                 |                                   |                                 |                       |                                 |                               |                               |                         |X                                    |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:stressDistFac](#IM:stressDistFac)|                           |                                     |                 |                                   |X                                |                       |                                 |                               |                               |                         |                                     |                 |X                                |                         |                       |                                 |                                     |                           |                           |
|[IM:nFL](#IM:nFL)                    |X                          |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |X                        |                       |                                 |                                     |                           |                           |
|[IM:dimlessLoad](#IM:dimlessLoad)    |X                          |                                     |X                |                                   |                                 |                       |X                                |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:tolLoad](#IM:tolLoad)            |                           |                                     |                 |                                   |X                                |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |X                      |                                 |                                     |                           |                           |
|[IM:sdfTol](#IM:sdfTol)              |X                          |X                                    |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:probOfBreak](#IM:probOfBreak)    |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |X                        |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:calofCapacity](#IM:calofCapacity)|                           |                                     |X                |                                   |                                 |                       |                                 |                               |                               |                         |                                     |X                |                                 |                         |                       |                                 |                                     |                           |                           |
|[IM:isSafePb](#IM:isSafePb)          |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |X                                |                                     |                           |X                          |
|[IM:isSafeLR](#IM:isSafeLR)          |                           |                                     |                 |                                   |                                 |                       |X                                |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |X                                    |X                          |                           |

**<p align="center">Traceability Matrix Showing the Connections Between Items and Other Sections</p>**

<div id="Table:TraceMatAllvsR"></div>

|                                                                         |[DD:minThick](#DD:minThick)|[DD:loadDurFactor](#DD:loadDurFactor)|[DD:gTF](#DD:gTF)|[DD:standOffDist](#DD:standOffDist)|[DD:aspectRatio](#DD:aspectRatio)|[DD:eqTNTW](#DD:eqTNTW)|[DD:calofDemand](#DD:calofDemand)|[TM:isSafeProb](#TM:isSafeProb)|[TM:isSafeLoad](#TM:isSafeLoad)|[IM:riskFun](#IM:riskFun)|[IM:stressDistFac](#IM:stressDistFac)|[IM:nFL](#IM:nFL)|[IM:dimlessLoad](#IM:dimlessLoad)|[IM:tolLoad](#IM:tolLoad)|[IM:sdfTol](#IM:sdfTol)|[IM:probOfBreak](#IM:probOfBreak)|[IM:calofCapacity](#IM:calofCapacity)|[IM:isSafePb](#IM:isSafePb)|[IM:isSafeLR](#IM:isSafeLR)|[FR:Input-Values](#inputValues)|[FR:System-Set-Values-Following-Assumptions](#sysSetValsFollowingAssumps)|[FR:Check-Input-with-Data_Constraints](#checkInputWithDataCons)|[FR:Output-Values-and-Known-Values](#outputValsAndKnownValues)|[FR:Check-Glass-Safety](#checkGlassSafety)|[FR:Output-Values](#outputValues)|[NFR:Correct](#correct)|[NFR:Verifiable](#verifiable)|[NFR:Understandable](#understandable)|[NFR:Reusable](#reusable)|[NFR:Maintainable](#maintainable)|[NFR:Portable](#portable)|
|:------------------------------------------------------------------------|:--------------------------|:------------------------------------|:----------------|:----------------------------------|:--------------------------------|:----------------------|:--------------------------------|:------------------------------|:------------------------------|:------------------------|:------------------------------------|:----------------|:--------------------------------|:------------------------|:----------------------|:--------------------------------|:------------------------------------|:--------------------------|:--------------------------|:------------------------------|:------------------------------------------------------------------------|:--------------------------------------------------------------|:-------------------------------------------------------------|:-----------------------------------------|:--------------------------------|:----------------------|:----------------------------|:------------------------------------|:------------------------|:--------------------------------|:------------------------|
|[GS:Predict-Glass-Withstands-Explosion](#willBreakGS)                    |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:Input-Values](#inputValues)                                          |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:System-Set-Values-Following-Assumptions](#sysSetValsFollowingAssumps)|                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:Check-Input-with-Data_Constraints](#checkInputWithDataCons)          |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:Output-Values-and-Known-Values](#outputValsAndKnownValues)           |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |X                              |X                                                                        |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:Check-Glass-Safety](#checkGlassSafety)                               |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |X                          |X                          |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[FR:Output-Values](#outputValues)                                        |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Correct](#correct)                                                  |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Verifiable](#verifiable)                                            |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Understandable](#understandable)                                    |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Reusable](#reusable)                                                |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Maintainable](#maintainable)                                        |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |
|[NFR:Portable](#portable)                                                |                           |                                     |                 |                                   |                                 |                       |                                 |                               |                               |                         |                                     |                 |                                 |                         |                       |                                 |                                     |                           |                           |                               |                                                                         |                                                               |                                                              |                                          |                                 |                       |                             |                                     |                         |                                 |                         |

**<p align="center">Traceability Matrix Showing the Connections Between Requirements, Goal Statements and Other Items</p>**

The purpose of the traceability graphs is also to provide easy references on what has to be additionally modified if a certain component is changed. The arrows in the graphs represent dependencies. The component at the tail of an arrow is depended on by the component at the head of that arrow. Therefore, if a component is changed, the components that it points to should also be changed. [Fig:TraceGraphAvsA](#Figure:TraceGraphAvsA) shows the dependencies of assumptions on each other. [Fig:TraceGraphAvsAll](#Figure:TraceGraphAvsAll) shows the dependencies of data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Fig:TraceGraphRefvsRef](#Figure:TraceGraphRefvsRef) shows the dependencies of data definitions, theoretical models, general definitions, and instance models on each other. [Fig:TraceGraphAllvsR](#Figure:TraceGraphAllvsR) shows the dependencies of requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models. [Fig:TraceGraphAllvsAll](#Figure:TraceGraphAllvsAll) shows the dependencies of dependencies of assumptions, models, definitions, requirements, goals, and changes with each other.

<div id="Figure:TraceGraphAvsA"></div>

![TraceGraphAvsA](/assets/avsa.svg)

**<p align="center">TraceGraphAvsA</p>**

<div id="Figure:TraceGraphAvsAll"></div>

![TraceGraphAvsAll](/assets/avsall.svg)

**<p align="center">TraceGraphAvsAll</p>**

<div id="Figure:TraceGraphRefvsRef"></div>

![TraceGraphRefvsRef](/assets/refvsref.svg)

**<p align="center">TraceGraphRefvsRef</p>**

<div id="Figure:TraceGraphAllvsR"></div>

![TraceGraphAllvsR](/assets/allvsr.svg)

**<p align="center">TraceGraphAllvsR</p>**

<div id="Figure:TraceGraphAllvsAll"></div>

![TraceGraphAllvsAll](/assets/allvsall.svg)

**<p align="center">TraceGraphAllvsAll</p>**

For convenience, the following graphs can be found at the links below:

- [TraceGraphAvsA](../../../../traceygraphs/glassbr/avsa.svg)
- [TraceGraphAvsAll](../../../../traceygraphs/glassbr/avsall.svg)
- [TraceGraphRefvsRef](../../../../traceygraphs/glassbr/refvsref.svg)
- [TraceGraphAllvsR](../../../../traceygraphs/glassbr/allvsr.svg)
- [TraceGraphAllvsAll](../../../../traceygraphs/glassbr/allvsall.svg)

# Values of Auxiliary Constants {#Sec:AuxConstants}

This section contains the standard values that are used for calculations in GlassBR.

<div id="Table:TAuxConsts"></div>

|Symbol                           |Description                                               |Value                    |Unit                                    |
|:--------------------------------|:---------------------------------------------------------|:------------------------|:---------------------------------------|
|\\({\mathit{AR}\_{\text{max}}}\\)|maximum aspect ratio                                      |\\(5\\)                  |--                                      |
|\\({d\_{\text{max}}}\\)          |maximum value for one of the dimensions of the glass plate|\\(5\\)                  |\\({\text{m}}\\)                        |
|\\({d\_{\text{min}}}\\)          |minimum value for one of the dimensions of the glass plate|\\(0.1\\)                |\\({\text{m}}\\)                        |
|\\(E\\)                          |modulus of elasticity of glass                            |\\(71.7\cdot{}10^{9}\\)  |\\({\text{Pa}}\\)                       |
|\\({J\_{\text{max}}}\\)          |maximum value for the stress distribution factor          |\\(32\\)                 |--                                      |
|\\({J\_{\text{min}}}\\)          |minimum value for the stress distribution factor          |\\(1\\)                  |--                                      |
|\\(k\\)                          |surface flaw parameter                                    |\\(28.6\cdot{}10^{-54}\\)|\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\(\mathit{LSF}\\)               |load share factor                                         |\\(1\\)                  |--                                      |
|\\(m\\)                          |surface flaw parameter                                    |\\(7\\)                  |\\(\frac{\text{m}^{12}}{\text{N}^{7}}\\)|
|\\({\mathit{SD}\_{\text{max}}}\\)|maximum stand off distance permissible for input          |\\(130\\)                |\\({\text{m}}\\)                        |
|\\({\mathit{SD}\_{\text{min}}}\\)|minimum stand off distance permissible for input          |\\(6\\)                  |\\({\text{m}}\\)                        |
|\\({t\_{\text{d}}}\\)            |duration of load                                          |\\(3\\)                  |\\({\text{s}}\\)                        |
|\\({w\_{\text{max}}}\\)          |maximum permissible input charge weight                   |\\(910\\)                |\\({\text{kg}}\\)                       |
|\\({w\_{\text{min}}}\\)          |minimum permissible input charge weight                   |\\(4.5\\)                |\\({\text{kg}}\\)                       |

**<p align="center">Auxiliary Constants</p>**

# References {#Sec:References}

<div id="astm2009"></div>

[1]: ASTM. *Standard Practice for Determining Load Resistance of Glass in Buildings*. *Standard E1300-09a*. ASTM International, 2009. <https://www.astm.org>.

<div id="astm2012"></div>

[2]: ASTM. *Standard Specification for Heat-Strengthened and Fully Tempered Flat Glass*. West Conshohocken, PA: ASTM International, 2012. <https://doi.org/10.1520/C1048-12E01>.

<div id="astm2016"></div>

[3]: ASTM. *Standard specification for Flat Glass*. West Conshohocken, PA: ASTM International, 2016. <https://doi.org/10.1520/C1036-16>.

<div id="beasonEtAl1998"></div>

[4]: Beason, W. Lynn, Kohutek, Terry L., and Bracci, Joseph M. *Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts*. *ASCE Library*. February, 1998. <https://doi.org/10.1061/(ASCE)0733-9445(1998)124:2(215)>.

<div id="campidelli"></div>

[5]: Campidelli, Manuel. "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading."

<div id="koothoor2013"></div>

[6]: Koothoor, Nirmitha. *A Document Driven Approach to Certifying Scientific Computing Software*. McMaster University, Hamilton, ON, Canada: 2013. Print.

<div id="parnasClements1986"></div>

[7]: Parnas, David L. and Clements, P. C. "A rational design process: How and why to fake it." *IEEE Transactions on Software Engineering*, vol. 12, no. 2, Washington, USA: February, 1986. pp. 251&ndash;257. Print.

<div id="smithKoothoor2016"></div>

[8]: Smith, W. Spencer and Koothoor, Nirmitha. "A Document-Driven Method for Certifying Scientific Computing Software for Use in Nuclear Safety Analysis." * Nuclear Engineering and Technology*, vol. 48, no. 2, April, 2016. <http://www.sciencedirect.com/science/article/pii/S1738573315002582>. pp. 404&ndash;418.

<div id="smithLai2005"></div>

[9]: Smith, W. Spencer and Lai, Lei. "A new requirements template for scientific computing." *Proceedings of the First International Workshop on Situational Requirements Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements Engineering Processes, SREP'05*. Edited by PJ Agerfalk, N. Kraiem, and J. Ralyte, Paris, France: 2005. pp. 107&ndash;121. In conjunction with 13th IEEE International Requirements Engineering Conference,

<div id="smithEtAl2007"></div>

[10]: Smith, W. Spencer, Lai, Lei, and Khedri, Ridha. "Requirements Analysis for Engineering Computation: A Systematic Approach for Improving Software Reliability." *Reliable Computing, Special Issue on Reliable Engineering Computation*, vol. 13, no. 1, February, 2007. <https://doi.org/10.1007/s11155-006-9020-7>. pp. 83&ndash;107.

# Appendix {#Sec:Appendix}

This appendix holds the graphs ([Fig:demandVSsod](#Figure:demandVSsod) and [Fig:dimlessloadVSaspect](#Figure:dimlessloadVSaspect)) used for interpolating values needed in the models.

<div id="Figure:demandVSsod"></div>

![3 second duration equivalent pressure (\\(q\\)) versus Stand off distance (SD) versus Charge weight (\\(w\\))](/assets/ASTM_F2248-09.png)

**<p align="center">3 second duration equivalent pressure (\\(q\\)) versus Stand off distance (SD) versus Charge weight (\\(w\\))</p>**

<div id="Figure:dimlessloadVSaspect"></div>

![Non dimensional lateral applied load (demand) or pressure (\\(\hat{q}\\)) versus Aspect Ratio (AR) versus Stress distribution factor (Function) (\\(J\\))](/assets/ASTM_F2248-09_BeasonEtAl.png)

**<p align="center">Non dimensional lateral applied load (demand) or pressure (\\(\hat{q}\\)) versus Aspect Ratio (AR) versus Stress distribution factor (Function) (\\(J\\))</p>**
