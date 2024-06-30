# Software Requirements Specification for Double Pendulum
Dong Chen

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
- [General System Description](#Sec:GenSysDesc)
  - [System Context](#Sec:SysContext)
  - [User Characteristics](#Sec:UserChars)
  - [System Constraints](#Sec:SysConstraints)
- [Specific System Description](#Sec:SpecSystDesc)
  - [Problem Description](#Sec:ProbDesc)
    - [Terminology and Definitions](#Sec:TermDefs)
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
- [Traceability Matrices and Graphs](#Sec:TraceMatrices)
- [Values of Auxiliary Constants](#Sec:AuxConstants)
- [References](#Sec:References)

# Reference Material {#Sec:RefMat}

This section records information for easy reference.

# Table of Units {#Sec:ToU}

The unit system used throughout is SI (Système International d'Unités). In addition to the basic units, several derived units are also used. For each unit, the [Table of Units](#Table:ToU) lists the symbol, a description, and the SI name.

<div id="Table:ToU"></div>

|Symbol            |Description|SI Name |
|:-----------------|:----------|:-------|
|\\({\text{kg}}\\) |mass       |kilogram|
|\\({\text{m}}\\)  |length     |metre   |
|\\({\text{N}}\\)  |force      |newton  |
|\\({\text{rad}}\\)|angle      |radian  |
|\\({\text{s}}\\)  |time       |second  |

**<p align="center">Table of Units</p>**

# Table of Symbols {#Sec:ToS}

The symbols used in this document are summarized in the [Table of Symbols](#Table:ToS) along with their units. Throughout the document, symbols in bold will represent vectors, and scalars otherwise. The symbols are listed in alphabetical order. For vector quantities, the units shown are for each component of the vector.

<div id="Table:ToS"></div>

|Symbol                               |Description                                      |Units                                |
|:------------------------------------|:------------------------------------------------|:------------------------------------|
|\\({a\_{\text{x}1}}\\)               |Horizontal acceleration of the first object      |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\({a\_{\text{x}2}}\\)               |Horizontal acceleration of the second object     |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\({a\_{\text{y}1}}\\)               |Vertical acceleration of the first object        |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\({a\_{\text{y}2}}\\)               |Vertical acceleration of the second object       |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\(\boldsymbol{a}\text{(}t\text{)}\\)|Acceleration                                     |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\(\boldsymbol{F}\\)                 |Force                                            |\\({\text{N}}\\)                     |
|\\(g\\)                              |Magnitude of gravitational acceleration          |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\(\boldsymbol{g}\\)                 |Gravitational acceleration                       |\\(\frac{\text{m}}{\text{s}^{2}}\\)  |
|\\(\boldsymbol{\hat{i}}\\)           |Unit vector                                      |--                                   |
|\\({L\_{1}}\\)                       |Length of the first rod                          |\\({\text{m}}\\)                     |
|\\({L\_{2}}\\)                       |Length of the second rod                         |\\({\text{m}}\\)                     |
|\\(m\\)                              |Mass                                             |\\({\text{kg}}\\)                    |
|\\({m\_{1}}\\)                       |Mass of the first object                         |\\({\text{kg}}\\)                    |
|\\({m\_{2}}\\)                       |Mass of the second object                        |\\({\text{kg}}\\)                    |
|\\({p\_{\text{x}1}}\\)               |Horizontal position of the first object          |\\({\text{m}}\\)                     |
|\\({p\_{\text{x}2}}\\)               |Horizontal position of the second object         |\\({\text{m}}\\)                     |
|\\({p\_{\text{y}1}}\\)               |Vertical position of the first object            |\\({\text{m}}\\)                     |
|\\({p\_{\text{y}2}}\\)               |Vertical position of the second object           |\\({\text{m}}\\)                     |
|\\(\boldsymbol{p}\text{(}t\text{)}\\)|Position                                         |\\({\text{m}}\\)                     |
|\\(\boldsymbol{T}\\)                 |Tension                                          |\\({\text{N}}\\)                     |
|\\({\boldsymbol{T}\_{1}}\\)          |Tension of the first object                      |\\({\text{N}}\\)                     |
|\\({\boldsymbol{T}\_{2}}\\)          |Tension of the second object                     |\\({\text{N}}\\)                     |
|\\(t\\)                              |Time                                             |\\({\text{s}}\\)                     |
|\\(\text{theta}\\)                   |Dependent variables                              |\\({\text{rad}}\\)                   |
|\\({v\_{\text{x}1}}\\)               |Horizontal velocity of the first object          |\\(\frac{\text{m}}{\text{s}}\\)      |
|\\({v\_{\text{x}2}}\\)               |Horizontal velocity of the second object         |\\(\frac{\text{m}}{\text{s}}\\)      |
|\\({v\_{\text{y}1}}\\)               |Vertical velocity of the first object            |\\(\frac{\text{m}}{\text{s}}\\)      |
|\\({v\_{\text{y}2}}\\)               |Vertical velocity of the second object           |\\(\frac{\text{m}}{\text{s}}\\)      |
|\\(\boldsymbol{v}\text{(}t\text{)}\\)|Velocity                                         |\\(\frac{\text{m}}{\text{s}}\\)      |
|\\({w\_{1}}\\)                       |Angular velocity of the first object             |\\(\frac{\text{rad}}{\text{s}}\\)    |
|\\({w\_{2}}\\)                       |Angular velocity of the second object            |\\(\frac{\text{rad}}{\text{s}}\\)    |
|\\({α\_{1}}\\)                       |Angular acceleration of the first object         |\\(\frac{\text{rad}}{\text{s}^{2}}\\)|
|\\({α\_{2}}\\)                       |Angular acceleration of the second object        |\\(\frac{\text{rad}}{\text{s}^{2}}\\)|
|\\({θ\_{1}}\\)                       |Angle of the first rod                           |\\({\text{rad}}\\)                   |
|\\({θ\_{2}}\\)                       |Angle of the second rod                          |\\({\text{rad}}\\)                   |
|\\(π\\)                              |Ratio of circumference to diameter for any circle|--                                   |

**<p align="center">Table of Symbols</p>**

# Abbreviations and Acronyms {#Sec:TAbbAcc}

<div id="Table:TAbbAcc"></div>

|Abbreviation|Full Form                          |
|:-----------|:----------------------------------|
|2D          |Two-Dimensional                    |
|A           |Assumption                         |
|DD          |Data Definition                    |
|DblPend     |Double Pendulum                    |
|GD          |General Definition                 |
|GS          |Goal Statement                     |
|IM          |Instance Model                     |
|PS          |Physical System Description        |
|R           |Requirement                        |
|RefBy       |Referenced by                      |
|Refname     |Reference Name                     |
|SRS         |Software Requirements Specification|
|TM          |Theoretical Model                  |
|Uncert.     |Typical Uncertainty                |

**<p align="center">Abbreviations and Acronyms</p>**

# Introduction {#Sec:Intro}

A pendulum consists of mass attached to the end of a rod and its moving curve is highly sensitive to initial conditions. Therefore, it is useful to have a program to simulate the motion of the pendulum to exhibit its chaotic characteristics. The program documented here is called Double Pendulum.

The following section provides an overview of the Software Requirements Specification (SRS) for Double Pendulum. This section explains the purpose of this document, the scope of the requirements, the characteristics of the intended reader, and the organization of the document.

# Purpose of Document {#Sec:DocPurpose}

The primary purpose of this document is to record the requirements of DblPend. Goals, assumptions, theoretical models, definitions, and other model derivation information are specified, allowing the reader to fully understand and verify the purpose and scientific basis of DblPend. With the exception of [system constraints](#Sec:SysConstraints), this SRS will remain abstract, describing what problem is being solved, but not how to solve it.

This document will be used as a starting point for subsequent development phases, including writing the design specification and the software verification and validation plan. The design document will show how the requirements are to be realized, including decisions on the numerical algorithms and programming environment. The verification and validation plan will show the steps that will be used to increase confidence in the software documentation and the implementation. Although the SRS fits in a series of documents that follow the so-called waterfall model, the actual development process is not constrained in any way. Even when the waterfall model is not followed, as Parnas and Clements point out [parnasClements1986](#parnasClements1986), the most logical way to present the documentation is still to "fake" a rational design process.

# Scope of Requirements {#Sec:ReqsScope}

The scope of the requirements includes the analysis of a two-dimensional (2D) pendulum motion problem with various initial conditions.

# Characteristics of Intended Reader {#Sec:ReaderChars}

Reviewers of this documentation should have an understanding of undergraduate level 2 physics, undergraduate level 1 calculus, and ordinary differential equations. The users of DblPend can have a lower level of expertise, as explained in [Sec:User Characteristics](#Sec:UserChars).

# Organization of Document {#Sec:DocOrg}

The organization of this document follows the template for an SRS for scientific computing software proposed by [koothoor2013](#koothoor2013), [smithLai2005](#smithLai2005), [smithEtAl2007](#smithEtAl2007), and [smithKoothoor2016](#smithKoothoor2016). The presentation follows the standard pattern of presenting goals, theories, definitions, and assumptions. For readers that would like a more bottom up approach, they can start reading the [instance models](#Sec:IMs) and trace back to find any additional information they require.

The [goal statements](#Sec:GoalStmt) are refined to the theoretical models and the [theoretical models](#Sec:TMs) to the [instance models](#Sec:IMs).

# General System Description {#Sec:GenSysDesc}

This section provides general information about the system. It identifies the interfaces between the system and its environment, describes the user characteristics, and lists the system constraints.

# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](#Figure:sysCtxDiag) shows the system context. A circle represents an entity external to the software, the user in this case. A rectangle represents the software system itself (DblPend). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag"></div>

![System Context](../../../../datafiles/dblpend/SystemContextFigure.png)
**<p align="center">System Context</p>**

The interaction between the product and the user is through an application programming interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide initial conditions of the physical state of the motion and the input data related to the Double Pendulum, ensuring no errors in the data entry.
  - Ensure that consistent units are used for input variables.
  - Ensure required [software assumptions](#Sec:Assumps) are appropriate for any particular problem input to the software.
- DblPend Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number.
  - Determine if the inputs satisfy the required physical and software constraints.
  - Calculate the required outputs.
  - Generate the required graphs.

# User Characteristics {#Sec:UserChars}

The end user of DblPend should have an understanding of high school physics, high school calculus and ordinary differential equations.

# System Constraints {#Sec:SysConstraints}

There are no system constraints.

# Specific System Description {#Sec:SpecSystDesc}

This section first presents the problem description, which gives a high-level view of the problem to be solved. This is followed by the solution characteristics specification, which presents the assumptions, theories, and definitions that are used.

# Problem Description {#Sec:ProbDesc}

A system is needed to predict the motion of a double pendulum.

# Terminology and Definitions {#Sec:TermDefs}

This subsection provides a list of terms that are used in the subsequent sections and their meaning, with the purpose of reducing ambiguity and making it easier to correctly understand the requirements.

- Gravity: The force that attracts one physical body with mass to another.
- Cartesian coordinate system: A coordinate system that specifies each point uniquely in a plane by a set of numerical coordinates, which are the signed distances to the point from two fixed perpendicular oriented lines, measured in the same unit of length (from [cartesianWiki](#cartesianWiki)).

# Physical System Description {#Sec:PhysSyst}

The physical system of DblPend, as shown in [Fig:dblpend](#Figure:dblpend), includes the following elements:

PS1: The first rod (with length of the first rod \\({L\_{1}}\\)).

PS2: The second rod (with length of the second rod \\({L\_{2}}\\)).

PS3: The first object.

PS4: The second object.


<div id="Figure:dblpend"></div>

![The physical system](../../../../datafiles/dblpend/dblpend.png)
**<p align="center">The physical system</p>**

# Goal Statements {#Sec:GoalStmt}

Given the masses, length of the rods, initial angle of the masses and the gravitational constant, the goal statement is:

<div id="motionMass"></div>

motionMass: Calculate the motion of the masses.


# Solution Characteristics Specification {#Sec:SolCharSpec}

The instance models that govern DblPend are presented in the [Instance Model Section](#Sec:IMs). The information to understand the meaning of the instance models and their derivation is also presented, so that the instance models can be verified.

# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="twoDMotion"></div>

twoDMotion: The pendulum motion is two-dimensional (2D).

<div id="cartSys"></div>

cartSys: A Cartesian coordinate system is used.

<div id="cartSysR"></div>

cartSysR: The Cartesian coordinate system is right-handed where positive \\(x\\)-axis and \\(y\\)-axis point right up.

<div id="yAxisDir"></div>

yAxisDir: The direction of the \\(y\\)-axis is directed opposite to gravity.

<div id="startOrigin"></div>

startOrigin: The first rod is attached to the origin.

<div id="firstPend"></div>

firstPend: The first rod has two sides. One side attaches to the origin. Another side attaches to the first object.

<div id="secondPend"></div>

secondPend: The second rod has two sides. One side attaches to the first object. Another side attaches to the second object.


# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that DblPend is based on.

<div align="center">

## Acceleration {#TM:acceleration}

</div>

|Refname    |TM:acceleration                                                                                                                                                                                                                                                 |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration                                                                                                                                                                                                                                                    |
|Equation   |\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |[accelerationWiki](#accelerationWiki)                                                                                                                                                                                                                           |
|RefBy      |                                                                                                                                                                                                                                                                |

<div align="center">

## Velocity {#TM:velocity}

</div>

|Refname    |TM:velocity                                                                                                                                                                                                                              |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity                                                                                                                                                                                                                                 |
|Equation   |\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                  |
|Description|<ul><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Source     |[velocityWiki](#velocityWiki)                                                                                                                                                                                                            |
|RefBy      |                                                                                                                                                                                                                                         |

<div align="center">

## Newton's second law of motion {#TM:NewtonSecLawMot}

</div>

|Refname    |TM:NewtonSecLawMot                                                                                                                                                                                                                  |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's second law of motion                                                                                                                                                                                                       |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}\\]                                                                                                                                                                              |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>      |
|Notes      |<ul><li>The net force \\(\boldsymbol{F}\\) on a body is proportional to the acceleration \\(\boldsymbol{a}\text{(}t\text{)}\\) of the body, where \\(m\\) denotes the mass of the body as the constant of proportionality.</li></ul>|
|Source     |--                                                                                                                                                                                                                                  |
|RefBy      |                                                                                                                                                                                                                                    |

# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## The \\(x\\)-component of velocity of the first object {#GD:velocityX1}

</div>

|Refname    |GD:velocityX1                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of velocity of the first object                                                                                                                                                                                                                                                                                                                              |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{v\_{\text{x}1}}={w\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\({v\_{\text{x}1}}\\) is the horizontal velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of the \\(x\\)-component of velocity: {#GD:velocityX1Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the horizontal position that is defined in [DD:positionXDD1](#DD:positionXDD1)

\\[{p\_{\text{x}1}}={L\_{1}} \sin\left({θ\_{1}}\right)\\]

Applying this,

\\[{v\_{\text{x}1}}=\frac{\\,d{L\_{1}} \sin\left({θ\_{1}}\right)}{\\,dt}\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{x}1}}={L\_{1}} \frac{\\,d\sin\left({θ\_{1}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{x}1}}={w\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(y\\)-component of velocity of the first object {#GD:velocityY1}

</div>

|Refname    |GD:velocityY1                                                                                                                                                                                                                                                                                                                                                                    |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of velocity of the first object                                                                                                                                                                                                                                                                                                                            |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                  |
|Equation   |\\[{v\_{\text{y}1}}={w\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                               |
|Description|<ul><li>\\({v\_{\text{y}1}}\\) is the vertical velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                               |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                 |

#### Detailed derivation of the \\(y\\)-component of velocity: {#GD:velocityY1Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the vertical position that is defined in [DD:positionYDD1](#DD:positionYDD1)

\\[{p\_{\text{y}1}}=-{L\_{1}} \cos\left({θ\_{1}}\right)\\]

Applying this,

\\[{v\_{\text{y}1}}=-\left(\frac{\\,d{L\_{1}} \cos\left({θ\_{1}}\right)}{\\,dt}\right)\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{y}1}}=-{L\_{1}} \frac{\\,d\cos\left({θ\_{1}}\right)}{\\,dt}\\]

Therefore, using the chain rule,

\\[{v\_{\text{y}1}}={w\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(x\\)-component of velocity of the second object {#GD:velocityX2}

</div>

|Refname    |GD:velocityX2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of velocity of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\({v\_{\text{x}2}}\\) is the horizontal velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({v\_{\text{x}1}}\\) is the horizontal velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of the \\(x\\)-component of velocity: {#GD:velocityX2Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the horizontal position that is defined in [DD:positionXDD2](#DD:positionXDD2)

\\[{p\_{\text{x}2}}={p\_{\text{x}1}}+{L\_{2}} \sin\left({θ\_{2}}\right)\\]

Applying this,

\\[{v\_{\text{x}2}}=\frac{\\,d{p\_{\text{x}1}}+{L\_{2}} \sin\left({θ\_{2}}\right)}{\\,dt}\\]

\\({L\_{1}}\\) is constant with respect to time, so

\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(y\\)-component of velocity of the second object {#GD:velocityY2}

</div>

|Refname    |GD:velocityY2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of velocity of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\({v\_{\text{y}2}}\\) is the vertical velocity of the second object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({v\_{\text{y}1}}\\) is the vertical velocity of the first object (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of the \\(y\\)-component of velocity: {#GD:velocityY2Deriv}

At a given point in time, velocity is defined in [DD:positionGDD](#DD:positionGDD)

\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]

We also know the vertical position that is defined in [DD:positionYDD2](#DD:positionYDD2)

\\[{p\_{\text{y}2}}={p\_{\text{y}1}}-{L\_{2}} \cos\left({θ\_{2}}\right)\\]

Applying this,

\\[{v\_{\text{y}2}}=-\left(\frac{\\,d{p\_{\text{y}1}}-{L\_{2}} \cos\left({θ\_{2}}\right)}{\\,dt}\right)\\]

Therefore, using the chain rule,

\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(x\\)-component of acceleration of the first object {#GD:accelerationX1}

</div>

|Refname    |GD:accelerationX1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of acceleration of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Equation   |\\[{a\_{\text{x}1}}=-{w\_{1}}^{2} {L\_{1}} \sin\left({θ\_{1}}\right)+{α\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                       |
|Description|<ul><li>\\({a\_{\text{x}1}}\\) is the horizontal acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |

#### Detailed derivation of the \\(x\\)-component of acceleration: {#GD:accelerationX1Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{x}1}}={w\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{x}1}}=\frac{\\,d{w\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{x}1}}=\frac{\\,d{w\_{1}}}{\\,dt} {L\_{1}} \cos\left({θ\_{1}}\right)-{w\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right) \frac{\\,d{θ\_{1}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{x}1}}=-{w\_{1}}^{2} {L\_{1}} \sin\left({θ\_{1}}\right)+{α\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(y\\)-component of acceleration of the first object {#GD:accelerationY1}

</div>

|Refname    |GD:accelerationY1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of acceleration of the first object                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[{a\_{\text{y}1}}={w\_{1}}^{2} {L\_{1}} \cos\left({θ\_{1}}\right)+{α\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                      |
|Description|<ul><li>\\({a\_{\text{y}1}}\\) is the vertical acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of the \\(y\\)-component of acceleration: {#GD:accelerationY1Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the vertical velocity to be

\\[{v\_{\text{y}1}}={w\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{y}1}}=\frac{\\,d{w\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{y}1}}=\frac{\\,d{w\_{1}}}{\\,dt} {L\_{1}} \sin\left({θ\_{1}}\right)+{w\_{1}} {L\_{1}} \cos\left({θ\_{1}}\right) \frac{\\,d{θ\_{1}}}{\\,dt}\\]

Simplifying,

\\[{a\_{\text{y}1}}={w\_{1}}^{2} {L\_{1}} \cos\left({θ\_{1}}\right)+{α\_{1}} {L\_{1}} \sin\left({θ\_{1}}\right)\\]

<div align="center">

## The \\(x\\)-component of acceleration of the second object {#GD:accelerationX2}

</div>

|Refname    |GD:accelerationX2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(x\\)-component of acceleration of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{a\_{\text{x}2}}={a\_{\text{x}1}}-{w\_{2}}^{2} {L\_{2}} \sin\left({θ\_{2}}\right)+{α\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\({a\_{\text{x}2}}\\) is the horizontal acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({a\_{\text{x}1}}\\) is the horizontal acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

#### Detailed derivation of the \\(x\\)-component of acceleration: {#GD:accelerationX2Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{x}2}}={v\_{\text{x}1}}+{w\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{x}2}}=\frac{\\,d{v\_{\text{x}1}}+{w\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{x}2}}={a\_{\text{x}1}}-{w\_{2}}^{2} {L\_{2}} \sin\left({θ\_{2}}\right)+{α\_{2}} {L\_{2}} \cos\left({θ\_{2}}\right)\\]

<div align="center">

## The \\(y\\)-component of acceleration of the second object {#GD:accelerationY2}

</div>

|Refname    |GD:accelerationY2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |The \\(y\\)-component of acceleration of the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Equation   |\\[{a\_{\text{y}2}}={a\_{\text{y}1}}+{w\_{2}}^{2} {L\_{2}} \cos\left({θ\_{2}}\right)+{α\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\({a\_{\text{y}2}}\\) is the vertical acceleration of the second object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({a\_{\text{y}1}}\\) is the vertical acceleration of the first object (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |

#### Detailed derivation of the \\(y\\)-component of acceleration: {#GD:accelerationY2Deriv}

Our acceleration is:

\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Earlier, we found the horizontal velocity to be

\\[{v\_{\text{y}2}}={v\_{\text{y}1}}+{w\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)\\]

Applying this to our equation for acceleration

\\[{a\_{\text{y}2}}=\frac{\\,d{v\_{\text{y}1}}+{w\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)}{\\,dt}\\]

By the product and chain rules, we find

\\[{a\_{\text{y}2}}={a\_{\text{y}1}}+{w\_{2}}^{2} {L\_{2}} \cos\left({θ\_{2}}\right)+{α\_{2}} {L\_{2}} \sin\left({θ\_{2}}\right)\\]

<div align="center">

## Horizontal force on the first object {#GD:xForce1}

</div>

|Refname    |GD:xForce1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal force on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{1}} \sin\left({θ\_{1}}\right)+{\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the tension of the first object (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of force on the first object: {#GD:xForce1Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{1}} \sin\left({θ\_{1}}\right)+{\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right)\\]

<div align="center">

## Vertical force on the first object {#GD:yForce1}

</div>

|Refname    |GD:yForce1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical force on the first object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{1}} \cos\left({θ\_{1}}\right)-{\boldsymbol{T}\_{2}} \cos\left({θ\_{2}}\right)-{m\_{1}} \boldsymbol{g}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{1}}\\) is the tension of the first object (\\({\text{N}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

#### Detailed derivation of force on the first object: {#GD:yForce1Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{1}} \cos\left({θ\_{1}}\right)-{\boldsymbol{T}\_{2}} \cos\left({θ\_{2}}\right)-{m\_{1}} \boldsymbol{g}\\]

<div align="center">

## Horizontal force on the second object {#GD:xForce2}

</div>

|Refname    |GD:xForce2                                                                                                                                                                                                                                                                                                                                                                                          |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal force on the second object                                                                                                                                                                                                                                                                                                                                                               |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                             |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                  |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                   |

#### Detailed derivation of force on the second object: {#GD:xForce2Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=-{\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right)\\]

<div align="center">

## Vertical force on the second object {#GD:yForce2}

</div>

|Refname    |GD:yForce2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical force on the second object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{2}} \cos\left({θ\_{2}}\right)-{m\_{2}} \boldsymbol{g}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{T}\_{2}}\\) is the tension of the second object (\\({\text{N}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|RefBy      |[IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |

#### Detailed derivation of force on the second object: {#GD:yForce2Deriv}

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}={\boldsymbol{T}\_{2}} \cos\left({θ\_{2}}\right)-{m\_{2}} \boldsymbol{g}\\]

# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Velocity {#DD:positionGDD}

</div>

|Refname    |DD:positionGDD                                                                                                                                                                                                                           |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Velocity                                                                                                                                                                                                                                 |
|Symbol     |\\(\boldsymbol{v}\text{(}t\text{)}\\)                                                                                                                                                                                                    |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                          |
|Equation   |\\[\boldsymbol{v}\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                  |
|Description|<ul><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                       |
|RefBy      |[GD:velocityY2](#GD:velocityY2), [GD:velocityY1](#GD:velocityY1), [GD:velocityX2](#GD:velocityX2), and [GD:velocityX1](#GD:velocityX1)                                                                                                   |

<div align="center">

## Horizontal position of the first object {#DD:positionXDD1}

</div>

|Refname    |DD:positionXDD1                                                                                                                                                                                                                                              |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal position of the first object                                                                                                                                                                                                                      |
|Symbol     |\\({p\_{\text{x}1}}\\)                                                                                                                                                                                                                                       |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                             |
|Equation   |\\[{p\_{\text{x}1}}={L\_{1}} \sin\left({θ\_{1}}\right)\\]                                                                                                                                                                                                    |
|Description|<ul><li>\\({p\_{\text{x}1}}\\) is the horizontal position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{x}1}}\\) is the horizontal position</li><li>\\({p\_{\text{x}1}}\\) is shown in [Fig:dblpend](#Figure:dblpend).</li></ul>                                                                                                               |
|Source     |--                                                                                                                                                                                                                                                           |
|RefBy      |[GD:velocityX1](#GD:velocityX1)                                                                                                                                                                                                                              |

<div align="center">

## Vertical position of the first object {#DD:positionYDD1}

</div>

|Refname    |DD:positionYDD1                                                                                                                                                                                                                                            |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical position of the first object                                                                                                                                                                                                                      |
|Symbol     |\\({p\_{\text{y}1}}\\)                                                                                                                                                                                                                                     |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                           |
|Equation   |\\[{p\_{\text{y}1}}=-{L\_{1}} \cos\left({θ\_{1}}\right)\\]                                                                                                                                                                                                 |
|Description|<ul><li>\\({p\_{\text{y}1}}\\) is the vertical position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{y}1}}\\) is the vertical position</li><li>\\({p\_{\text{y}1}}\\) is shown in [Fig:dblpend](#Figure:dblpend).</li></ul>                                                                                                               |
|Source     |--                                                                                                                                                                                                                                                         |
|RefBy      |[GD:velocityY1](#GD:velocityY1)                                                                                                                                                                                                                            |

<div align="center">

## Horizontal position of the second object {#DD:positionXDD2}

</div>

|Refname    |DD:positionXDD2                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Horizontal position of the second object                                                                                                                                                                                                                                                                                                                         |
|Symbol     |\\({p\_{\text{x}2}}\\)                                                                                                                                                                                                                                                                                                                                           |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{p\_{\text{x}2}}={p\_{\text{x}1}}+{L\_{2}} \sin\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                       |
|Description|<ul><li>\\({p\_{\text{x}2}}\\) is the horizontal position of the second object (\\({\text{m}}\\))</li><li>\\({p\_{\text{x}1}}\\) is the horizontal position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{x}2}}\\) is the horizontal position</li><li>\\({p\_{\text{x}2}}\\) is shown in [Fig:dblpend](#Figure:dblpend).</li></ul>                                                                                                                                                                                                                   |
|Source     |--                                                                                                                                                                                                                                                                                                                                                               |
|RefBy      |[GD:velocityX2](#GD:velocityX2)                                                                                                                                                                                                                                                                                                                                  |

<div align="center">

## Vertical position of the second object {#DD:positionYDD2}

</div>

|Refname    |DD:positionYDD2                                                                                                                                                                                                                                                                                                                                              |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Vertical position of the second object                                                                                                                                                                                                                                                                                                                       |
|Symbol     |\\({p\_{\text{y}2}}\\)                                                                                                                                                                                                                                                                                                                                       |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[{p\_{\text{y}2}}={p\_{\text{y}1}}-{L\_{2}} \cos\left({θ\_{2}}\right)\\]                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\({p\_{\text{y}2}}\\) is the vertical position of the second object (\\({\text{m}}\\))</li><li>\\({p\_{\text{y}1}}\\) is the vertical position of the first object (\\({\text{m}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>\\({p\_{\text{y}2}}\\) is the vertical position</li><li>\\({p\_{\text{y}2}}\\) is shown in [Fig:dblpend](#Figure:dblpend).</li></ul>                                                                                                                                                                                                                 |
|Source     |--                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |[GD:velocityY2](#GD:velocityY2)                                                                                                                                                                                                                                                                                                                              |

<div align="center">

## Acceleration {#DD:accelerationGDD}

</div>

|Refname    |DD:accelerationGDD                                                                                                                                                                                                                                              |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration                                                                                                                                                                                                                                                    |
|Symbol     |\\(\boldsymbol{a}\text{(}t\text{)}\\)                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                             |
|Equation   |\\[\boldsymbol{a}\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                                                              |
|RefBy      |                                                                                                                                                                                                                                                                |

<div align="center">

## Force {#DD:forceGDD}

</div>

|Refname    |DD:forceGDD                                                                                                                                                                                                                   |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Force                                                                                                                                                                                                                         |
|Symbol     |\\(\boldsymbol{F}\\)                                                                                                                                                                                                          |
|Units      |\\({\text{N}}\\)                                                                                                                                                                                                              |
|Equation   |\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}\\]                                                                                                                                                                        |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{a}\text{(}t\text{)}\\) is the acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li></ul>|
|Source     |--                                                                                                                                                                                                                            |
|RefBy      |                                                                                                                                                                                                                              |

# Instance Models {#Sec:IMs}

This section transforms the problem defined in the [problem description](#Sec:ProbDesc) into one which is expressed in mathematical terms. It uses concrete symbols defined in the [data definitions](#Sec:DDs) to replace the abstract symbols in the models identified in [theoretical models](#Sec:TMs) and [general definitions](#Sec:GDs).

<div align="center">

## Calculation of angle of first rod {#IM:calOfAngle1}

</div>

|Refname           |IM:calOfAngle1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Calculation of angle of first rod                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Input             |\\({L\_{1}}\\), \\({L\_{2}}\\), \\({m\_{1}}\\), \\({m\_{2}}\\), \\({θ\_{1}}\\), \\({θ\_{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Output            |\\({θ\_{1}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Input Constraints |\\[{L\_{1}}\gt{}0\\]\\[{L\_{2}}\gt{}0\\]\\[{m\_{1}}\gt{}0\\]\\[{m\_{2}}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Equation          |\\[{α\_{1}}\left({θ\_{1}},{θ\_{2}},{w\_{1}},{w\_{2}}\right)=\frac{-g \left(2 {m\_{1}}+{m\_{2}}\right) \sin\left({θ\_{1}}\right)-{m\_{2}} g \sin\left({θ\_{1}}-2 {θ\_{2}}\right)-2 \sin\left({θ\_{1}}-{θ\_{2}}\right) {m\_{2}} \left({w\_{2}}^{2} {L\_{2}}+{w\_{1}}^{2} {L\_{1}} \cos\left({θ\_{1}}-{θ\_{2}}\right)\right)}{{L\_{1}} \left(2 {m\_{1}}+{m\_{2}}-{m\_{2}} \cos\left(2 {θ\_{1}}-2 {θ\_{2}}\right)\right)}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|Description       |<ul><li>\\({α\_{1}}\\) is the angular acceleration of the first object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li></ul>|
|Notes             |<ul><li>\\({θ\_{1}}\\) is calculated by solving the ODE here together with the initial conditions and [IM:calOfAngle2](#IM:calOfAngle2).</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Source            |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|RefBy             |[FR:Output-Values](#outputValues), [FR:Calculate-Angle-Of-Rod](#calcAng), and [IM:calOfAngle2](#IM:calOfAngle2)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

<div align="center">

## Calculation of angle of second rod {#IM:calOfAngle2}

</div>

|Refname           |IM:calOfAngle2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-----------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Calculation of angle of second rod                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Input             |\\({L\_{1}}\\), \\({L\_{2}}\\), \\({m\_{1}}\\), \\({m\_{2}}\\), \\({θ\_{1}}\\), \\({θ\_{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Output            |\\({θ\_{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Input Constraints |\\[{L\_{1}}\gt{}0\\]\\[{L\_{2}}\gt{}0\\]\\[{m\_{1}}\gt{}0\\]\\[{m\_{2}}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Equation          |\\[{α\_{2}}\left({θ\_{1}},{θ\_{2}},{w\_{1}},{w\_{2}}\right)=\frac{2 \sin\left({θ\_{1}}-{θ\_{2}}\right) \left({w\_{1}}^{2} {L\_{1}} \left({m\_{1}}+{m\_{2}}\right)+g \left({m\_{1}}+{m\_{2}}\right) \cos\left({θ\_{1}}\right)+{w\_{2}}^{2} {L\_{2}} {m\_{2}} \cos\left({θ\_{1}}-{θ\_{2}}\right)\right)}{{L\_{2}} \left(2 {m\_{1}}+{m\_{2}}-{m\_{2}} \cos\left(2 {θ\_{1}}-2 {θ\_{2}}\right)\right)}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Description       |<ul><li>\\({α\_{2}}\\) is the angular acceleration of the second object (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\({θ\_{1}}\\) is the angle of the first rod (\\({\text{rad}}\\))</li><li>\\({θ\_{2}}\\) is the angle of the second rod (\\({\text{rad}}\\))</li><li>\\({w\_{1}}\\) is the angular velocity of the first object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({w\_{2}}\\) is the angular velocity of the second object (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({L\_{1}}\\) is the length of the first rod (\\({\text{m}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first object (\\({\text{kg}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second object (\\({\text{kg}}\\))</li><li>\\(g\\) is the magnitude of gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({L\_{2}}\\) is the length of the second rod (\\({\text{m}}\\))</li></ul>|
|Notes             |<ul><li>\\({θ\_{2}}\\) is calculated by solving the ODE here together with the initial conditions and [IM:calOfAngle1](#IM:calOfAngle1).</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Source            |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|RefBy             |[FR:Output-Values](#outputValues), [FR:Calculate-Angle-Of-Rod](#calcAng), [IM:calOfAngle2](#IM:calOfAngle2), and [IM:calOfAngle1](#IM:calOfAngle1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |

#### Detailed derivation of angle of the second rod: {#IM:calOfAngle2Deriv}

By solving equations [GD:xForce2](#GD:xForce2) and [GD:yForce2](#GD:yForce2) for \\({\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right)\\) and \\({\boldsymbol{T}\_{2}} \cos\left({θ\_{2}}\right)\\) and then substituting into equation [GD:xForce1](#GD:xForce1) and [GD:yForce1](#GD:yForce1) , We can get equations 1 and 2:

\\[{m\_{1}} {a\_{\text{x}1}}=-{\boldsymbol{T}\_{1}} \sin\left({θ\_{1}}\right)-{m\_{2}} {a\_{\text{x}2}}\\]



\\[{m\_{1}} {a\_{\text{y}1}}={\boldsymbol{T}\_{1}} \cos\left({θ\_{1}}\right)-{m\_{2}} {a\_{\text{y}2}}-{m\_{2}} g-{m\_{1}} g\\]

Multiply the equation 1 by \\(\cos\left({θ\_{1}}\right)\\) and the equation 2 by \\(\sin\left({θ\_{1}}\right)\\) and rearrange to get:

\\[{\boldsymbol{T}\_{1}} \sin\left({θ\_{1}}\right) \cos\left({θ\_{1}}\right)=-\cos\left({θ\_{1}}\right) \left({m\_{1}} {a\_{\text{x}1}}+{m\_{2}} {a\_{\text{x}2}}\right)\\]



\\[{\boldsymbol{T}\_{1}} \sin\left({θ\_{1}}\right) \cos\left({θ\_{1}}\right)=\sin\left({θ\_{1}}\right) \left({m\_{1}} {a\_{\text{y}1}}+{m\_{2}} {a\_{\text{y}2}}+{m\_{2}} g+{m\_{1}} g\right)\\]

This leads to the equation 3

\\[\sin\left({θ\_{1}}\right) \left({m\_{1}} {a\_{\text{y}1}}+{m\_{2}} {a\_{\text{y}2}}+{m\_{2}} g+{m\_{1}} g\right)=-\cos\left({θ\_{1}}\right) \left({m\_{1}} {a\_{\text{x}1}}+{m\_{2}} {a\_{\text{x}2}}\right)\\]

Next, multiply equation [GD:xForce2](#GD:xForce2) by \\(\cos\left({θ\_{2}}\right)\\) and equation [GD:yForce2](#GD:yForce2) by \\(\sin\left({θ\_{2}}\right)\\) and rearrange to get:

\\[{\boldsymbol{T}\_{2}} \sin\left({θ\_{2}}\right) \cos\left({θ\_{2}}\right)=-\cos\left({θ\_{2}}\right) {m\_{2}} {a\_{\text{x}2}}\\]



\\[{\boldsymbol{T}\_{1}} \sin\left({θ\_{2}}\right) \cos\left({θ\_{2}}\right)=\sin\left({θ\_{2}}\right) \left({m\_{2}} {a\_{\text{y}2}}+{m\_{2}} g\right)\\]

which leads to equation 4

\\[\sin\left({θ\_{2}}\right) \left({m\_{2}} {a\_{\text{y}2}}+{m\_{2}} g\right)=-\cos\left({θ\_{2}}\right) {m\_{2}} {a\_{\text{x}2}}\\]

By giving equations [GD:accelerationX1](#GD:accelerationX1) and [GD:accelerationX2](#GD:accelerationX2) and [GD:accelerationY1](#GD:accelerationY1) and [GD:accelerationY2](#GD:accelerationY2) plus additional two equations, 3 and 4, we can get [IM:calOfAngle1](#IM:calOfAngle1) and [IM:calOfAngle2](#IM:calOfAngle2) via a computer algebra program:

# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario.

<div id="Table:InDataConstraints"></div>

|Var           |Physical Constraints|Typical Value              |Uncert.    |
|:-------------|:-------------------|:--------------------------|:----------|
|\\({L\_{1}}\\)|\\({L\_{1}}\gt{}0\\)|\\(1.0\\) \\({\text{m}}\\) |10\\(\\%\\)|
|\\({L\_{2}}\\)|\\({L\_{2}}\gt{}0\\)|\\(1.0\\) \\({\text{m}}\\) |10\\(\\%\\)|
|\\({m\_{1}}\\)|\\({m\_{1}}\gt{}0\\)|\\(0.5\\) \\({\text{kg}}\\)|10\\(\\%\\)|
|\\({m\_{2}}\\)|\\({m\_{2}}\gt{}0\\)|\\(0.5\\) \\({\text{kg}}\\)|10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**

# Properties of a Correct Solution {#Sec:CorSolProps}

The [Data Constraints Table](#Table:OutDataConstraints) shows the data constraints on the output variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable.

<div id="Table:OutDataConstraints"></div>

|Var           |Physical Constraints|
|:-------------|:-------------------|
|\\({θ\_{1}}\\)|\\({θ\_{1}}\gt{}0\\)|
|\\({θ\_{2}}\\)|\\({θ\_{2}}\gt{}0\\)|

**<p align="center">Output Data Constraints</p>**

# Requirements {#Sec:Requirements}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete, and the non-functional requirements, the qualities that the software is expected to exhibit.

# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="inputValues"></div>

Input-Values: Input the values from [Tab:ReqInputs](#Table:ReqInputs).

<div id="verifyInptVals"></div>

Verify-Input-Values: Check the entered input values to ensure that they do not exceed the [data constraints](#Sec:DataConstraints). If any of the input values are out of bounds, an error message is displayed and the calculations stop.

<div id="calcAng"></div>

Calculate-Angle-Of-Rod: Calculate the following values: \\({θ\_{1}}\\) and \\({θ\_{2}}\\) (from [IM:calOfAngle1](#IM:calOfAngle1) and [IM:calOfAngle2](#IM:calOfAngle2)).

<div id="outputValues"></div>

Output-Values: Output \\({θ\_{1}}\\) and \\({θ\_{2}}\\) (from [IM:calOfAngle1](#IM:calOfAngle1) and [IM:calOfAngle2](#IM:calOfAngle2)).


<div id="Table:ReqInputs"></div>

|Symbol        |Description              |Units            |
|:-------------|:------------------------|:----------------|
|\\({L\_{1}}\\)|Length of the first rod  |\\({\text{m}}\\) |
|\\({L\_{2}}\\)|Length of the second rod |\\({\text{m}}\\) |
|\\({m\_{1}}\\)|Mass of the first object |\\({\text{kg}}\\)|
|\\({m\_{2}}\\)|Mass of the second object|\\({\text{kg}}\\)|

**<p align="center">Required Inputs following [FR:Input-Values](#inputValues)</p>**

# Non-Functional Requirements {#Sec:NFRs}

This section provides the non-functional requirements, the qualities that the software is expected to exhibit.

<div id="correct"></div>

Correct: The outputs of the code have the [properties of a correct solution](#Sec:CorSolProps).

<div id="portable"></div>

Portable: The code is able to be run in different environments.


# Traceability Matrices and Graphs {#Sec:TraceMatrices}

The purpose of the traceability matrices is to provide easy references on what has to be additionally modified if a certain component is changed. Every time a component is changed, the items in the column of that component that are marked with an "X" should be modified as well. [Tab:TraceMatAvsA](#Table:TraceMatAvsA) shows the dependencies of the assumptions on each other. [Tab:TraceMatAvsAll](#Table:TraceMatAvsAll) shows the dependencies of the data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Tab:TraceMatRefvsRef](#Table:TraceMatRefvsRef) shows the dependencies of the data definitions, theoretical models, general definitions, and instance models on each other. [Tab:TraceMatAllvsR](#Table:TraceMatAllvsR) shows the dependencies of the requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models.

<div id="Table:TraceMatAvsA"></div>

|                             |[A:twoDMotion](#twoDMotion)|[A:cartSys](#cartSys)|[A:cartSysR](#cartSysR)|[A:yAxisDir](#yAxisDir)|[A:startOrigin](#startOrigin)|[A:firstPend](#firstPend)|[A:secondPend](#secondPend)|
|:----------------------------|:--------------------------|:--------------------|:----------------------|:----------------------|:----------------------------|:------------------------|:--------------------------|
|[A:twoDMotion](#twoDMotion)  |                           |                     |                       |                       |                             |                         |                           |
|[A:cartSys](#cartSys)        |                           |                     |                       |                       |                             |                         |                           |
|[A:cartSysR](#cartSysR)      |                           |                     |                       |                       |                             |                         |                           |
|[A:yAxisDir](#yAxisDir)      |                           |                     |                       |                       |                             |                         |                           |
|[A:startOrigin](#startOrigin)|                           |                     |                       |                       |                             |                         |                           |
|[A:firstPend](#firstPend)    |                           |                     |                       |                       |                             |                         |                           |
|[A:secondPend](#secondPend)  |                           |                     |                       |                       |                             |                         |                           |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Assumptions</p>**

<div id="Table:TraceMatAvsAll"></div>

|                                         |[A:twoDMotion](#twoDMotion)|[A:cartSys](#cartSys)|[A:cartSysR](#cartSysR)|[A:yAxisDir](#yAxisDir)|[A:startOrigin](#startOrigin)|[A:firstPend](#firstPend)|[A:secondPend](#secondPend)|
|:----------------------------------------|:--------------------------|:--------------------|:----------------------|:----------------------|:----------------------------|:------------------------|:--------------------------|
|[DD:positionGDD](#DD:positionGDD)        |                           |                     |                       |                       |                             |                         |                           |
|[DD:positionXDD1](#DD:positionXDD1)      |                           |                     |                       |                       |                             |                         |                           |
|[DD:positionYDD1](#DD:positionYDD1)      |                           |                     |                       |                       |                             |                         |                           |
|[DD:positionXDD2](#DD:positionXDD2)      |                           |                     |                       |                       |                             |                         |                           |
|[DD:positionYDD2](#DD:positionYDD2)      |                           |                     |                       |                       |                             |                         |                           |
|[DD:accelerationGDD](#DD:accelerationGDD)|                           |                     |                       |                       |                             |                         |                           |
|[DD:forceGDD](#DD:forceGDD)              |                           |                     |                       |                       |                             |                         |                           |
|[TM:acceleration](#TM:acceleration)      |                           |                     |                       |                       |                             |                         |                           |
|[TM:velocity](#TM:velocity)              |                           |                     |                       |                       |                             |                         |                           |
|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|                           |                     |                       |                       |                             |                         |                           |
|[GD:velocityX1](#GD:velocityX1)          |                           |                     |                       |                       |                             |                         |                           |
|[GD:velocityY1](#GD:velocityY1)          |                           |                     |                       |                       |                             |                         |                           |
|[GD:velocityX2](#GD:velocityX2)          |                           |                     |                       |                       |                             |                         |                           |
|[GD:velocityY2](#GD:velocityY2)          |                           |                     |                       |                       |                             |                         |                           |
|[GD:accelerationX1](#GD:accelerationX1)  |                           |                     |                       |                       |                             |                         |                           |
|[GD:accelerationY1](#GD:accelerationY1)  |                           |                     |                       |                       |                             |                         |                           |
|[GD:accelerationX2](#GD:accelerationX2)  |                           |                     |                       |                       |                             |                         |                           |
|[GD:accelerationY2](#GD:accelerationY2)  |                           |                     |                       |                       |                             |                         |                           |
|[GD:xForce1](#GD:xForce1)                |                           |                     |                       |                       |                             |                         |                           |
|[GD:yForce1](#GD:yForce1)                |                           |                     |                       |                       |                             |                         |                           |
|[GD:xForce2](#GD:xForce2)                |                           |                     |                       |                       |                             |                         |                           |
|[GD:yForce2](#GD:yForce2)                |                           |                     |                       |                       |                             |                         |                           |
|[IM:calOfAngle1](#IM:calOfAngle1)        |                           |                     |                       |                       |                             |                         |                           |
|[IM:calOfAngle2](#IM:calOfAngle2)        |                           |                     |                       |                       |                             |                         |                           |
|[FR:Input-Values](#inputValues)          |                           |                     |                       |                       |                             |                         |                           |
|[FR:Verify-Input-Values](#verifyInptVals)|                           |                     |                       |                       |                             |                         |                           |
|[FR:Calculate-Angle-Of-Rod](#calcAng)    |                           |                     |                       |                       |                             |                         |                           |
|[FR:Output-Values](#outputValues)        |                           |                     |                       |                       |                             |                         |                           |
|[NFR:Correct](#correct)                  |                           |                     |                       |                       |                             |                         |                           |
|[NFR:Portable](#portable)                |                           |                     |                       |                       |                             |                         |                           |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Items</p>**

<div id="Table:TraceMatRefvsRef"></div>

|                                         |[DD:positionGDD](#DD:positionGDD)|[DD:positionXDD1](#DD:positionXDD1)|[DD:positionYDD1](#DD:positionYDD1)|[DD:positionXDD2](#DD:positionXDD2)|[DD:positionYDD2](#DD:positionYDD2)|[DD:accelerationGDD](#DD:accelerationGDD)|[DD:forceGDD](#DD:forceGDD)|[TM:acceleration](#TM:acceleration)|[TM:velocity](#TM:velocity)|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|[GD:velocityX1](#GD:velocityX1)|[GD:velocityY1](#GD:velocityY1)|[GD:velocityX2](#GD:velocityX2)|[GD:velocityY2](#GD:velocityY2)|[GD:accelerationX1](#GD:accelerationX1)|[GD:accelerationY1](#GD:accelerationY1)|[GD:accelerationX2](#GD:accelerationX2)|[GD:accelerationY2](#GD:accelerationY2)|[GD:xForce1](#GD:xForce1)|[GD:yForce1](#GD:yForce1)|[GD:xForce2](#GD:xForce2)|[GD:yForce2](#GD:yForce2)|[IM:calOfAngle1](#IM:calOfAngle1)|[IM:calOfAngle2](#IM:calOfAngle2)|
|:----------------------------------------|:--------------------------------|:----------------------------------|:----------------------------------|:----------------------------------|:----------------------------------|:----------------------------------------|:--------------------------|:----------------------------------|:--------------------------|:----------------------------------------|:------------------------------|:------------------------------|:------------------------------|:------------------------------|:--------------------------------------|:--------------------------------------|:--------------------------------------|:--------------------------------------|:------------------------|:------------------------|:------------------------|:------------------------|:--------------------------------|:--------------------------------|
|[DD:positionGDD](#DD:positionGDD)        |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:positionXDD1](#DD:positionXDD1)      |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:positionYDD1](#DD:positionYDD1)      |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:positionXDD2](#DD:positionXDD2)      |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:positionYDD2](#DD:positionYDD2)      |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:accelerationGDD](#DD:accelerationGDD)|                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[DD:forceGDD](#DD:forceGDD)              |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[TM:acceleration](#TM:acceleration)      |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[TM:velocity](#TM:velocity)              |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:velocityX1](#GD:velocityX1)          |X                                |X                                  |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:velocityY1](#GD:velocityY1)          |X                                |                                   |X                                  |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:velocityX2](#GD:velocityX2)          |X                                |                                   |                                   |X                                  |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:velocityY2](#GD:velocityY2)          |X                                |                                   |                                   |                                   |X                                  |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:accelerationX1](#GD:accelerationX1)  |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:accelerationY1](#GD:accelerationY1)  |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:accelerationX2](#GD:accelerationX2)  |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:accelerationY2](#GD:accelerationY2)  |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:xForce1](#GD:xForce1)                |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:yForce1](#GD:yForce1)                |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:xForce2](#GD:xForce2)                |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[GD:yForce2](#GD:yForce2)                |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |
|[IM:calOfAngle1](#IM:calOfAngle1)        |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |X                                |
|[IM:calOfAngle2](#IM:calOfAngle2)        |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |X                                      |X                                      |X                                      |X                                      |X                        |X                        |X                        |X                        |X                                |X                                |

**<p align="center">Traceability Matrix Showing the Connections Between Items and Other Sections</p>**

<div id="Table:TraceMatAllvsR"></div>

|                                         |[DD:positionGDD](#DD:positionGDD)|[DD:positionXDD1](#DD:positionXDD1)|[DD:positionYDD1](#DD:positionYDD1)|[DD:positionXDD2](#DD:positionXDD2)|[DD:positionYDD2](#DD:positionYDD2)|[DD:accelerationGDD](#DD:accelerationGDD)|[DD:forceGDD](#DD:forceGDD)|[TM:acceleration](#TM:acceleration)|[TM:velocity](#TM:velocity)|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|[GD:velocityX1](#GD:velocityX1)|[GD:velocityY1](#GD:velocityY1)|[GD:velocityX2](#GD:velocityX2)|[GD:velocityY2](#GD:velocityY2)|[GD:accelerationX1](#GD:accelerationX1)|[GD:accelerationY1](#GD:accelerationY1)|[GD:accelerationX2](#GD:accelerationX2)|[GD:accelerationY2](#GD:accelerationY2)|[GD:xForce1](#GD:xForce1)|[GD:yForce1](#GD:yForce1)|[GD:xForce2](#GD:xForce2)|[GD:yForce2](#GD:yForce2)|[IM:calOfAngle1](#IM:calOfAngle1)|[IM:calOfAngle2](#IM:calOfAngle2)|[FR:Input-Values](#inputValues)|[FR:Verify-Input-Values](#verifyInptVals)|[FR:Calculate-Angle-Of-Rod](#calcAng)|[FR:Output-Values](#outputValues)|[NFR:Correct](#correct)|[NFR:Portable](#portable)|
|:----------------------------------------|:--------------------------------|:----------------------------------|:----------------------------------|:----------------------------------|:----------------------------------|:----------------------------------------|:--------------------------|:----------------------------------|:--------------------------|:----------------------------------------|:------------------------------|:------------------------------|:------------------------------|:------------------------------|:--------------------------------------|:--------------------------------------|:--------------------------------------|:--------------------------------------|:------------------------|:------------------------|:------------------------|:------------------------|:--------------------------------|:--------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:--------------------------------|:----------------------|:------------------------|
|[GS:motionMass](#motionMass)             |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |                               |                                         |                                     |                                 |                       |                         |
|[FR:Input-Values](#inputValues)          |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |                               |                                         |                                     |                                 |                       |                         |
|[FR:Verify-Input-Values](#verifyInptVals)|                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |                               |                                         |                                     |                                 |                       |                         |
|[FR:Calculate-Angle-Of-Rod](#calcAng)    |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |X                                |X                                |                               |                                         |                                     |                                 |                       |                         |
|[FR:Output-Values](#outputValues)        |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |X                                |X                                |                               |                                         |                                     |                                 |                       |                         |
|[NFR:Correct](#correct)                  |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |                               |                                         |                                     |                                 |                       |                         |
|[NFR:Portable](#portable)                |                                 |                                   |                                   |                                   |                                   |                                         |                           |                                   |                           |                                         |                               |                               |                               |                               |                                       |                                       |                                       |                                       |                         |                         |                         |                         |                                 |                                 |                               |                                         |                                     |                                 |                       |                         |

**<p align="center">Traceability Matrix Showing the Connections Between Requirements, Goal Statements and Other Items</p>**

The purpose of the traceability graphs is also to provide easy references on what has to be additionally modified if a certain component is changed. The arrows in the graphs represent dependencies. The component at the tail of an arrow is depended on by the component at the head of that arrow. Therefore, if a component is changed, the components that it points to should also be changed. [Fig:TraceGraphAvsA](#Figure:TraceGraphAvsA) shows the dependencies of assumptions on each other. [Fig:TraceGraphAvsAll](#Figure:TraceGraphAvsAll) shows the dependencies of data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Fig:TraceGraphRefvsRef](#Figure:TraceGraphRefvsRef) shows the dependencies of data definitions, theoretical models, general definitions, and instance models on each other. [Fig:TraceGraphAllvsR](#Figure:TraceGraphAllvsR) shows the dependencies of requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models. [Fig:TraceGraphAllvsAll](#Figure:TraceGraphAllvsAll) shows the dependencies of dependencies of assumptions, models, definitions, requirements, goals, and changes with each other.

<div id="Figure:TraceGraphAvsA"></div>

![TraceGraphAvsA](../../../../traceygraphs/dblpend/avsa.svg)
**<p align="center">TraceGraphAvsA</p>**

<div id="Figure:TraceGraphAvsAll"></div>

![TraceGraphAvsAll](../../../../traceygraphs/dblpend/avsall.svg)
**<p align="center">TraceGraphAvsAll</p>**

<div id="Figure:TraceGraphRefvsRef"></div>

![TraceGraphRefvsRef](../../../../traceygraphs/dblpend/refvsref.svg)
**<p align="center">TraceGraphRefvsRef</p>**

<div id="Figure:TraceGraphAllvsR"></div>

![TraceGraphAllvsR](../../../../traceygraphs/dblpend/allvsr.svg)
**<p align="center">TraceGraphAllvsR</p>**

<div id="Figure:TraceGraphAllvsAll"></div>

![TraceGraphAllvsAll](../../../../traceygraphs/dblpend/allvsall.svg)
**<p align="center">TraceGraphAllvsAll</p>**

For convenience, the following graphs can be found at the links below:

- [TraceGraphAvsA](../../../../traceygraphs/dblpend/avsa.svg)
- [TraceGraphAvsAll](../../../../traceygraphs/dblpend/avsall.svg)
- [TraceGraphRefvsRef](../../../../traceygraphs/dblpend/refvsref.svg)
- [TraceGraphAllvsR](../../../../traceygraphs/dblpend/allvsr.svg)
- [TraceGraphAllvsAll](../../../../traceygraphs/dblpend/allvsall.svg)

# Values of Auxiliary Constants {#Sec:AuxConstants}

There are no auxiliary constants.

# References {#Sec:References}

<div id="hibbeler2004"></div>

[1]: Hibbeler, R. C. *Engineering Mechanics: Dynamics*. Pearson Prentice Hall, 2004. Print.

<div id="koothoor2013"></div>

[2]: Koothoor, Nirmitha. *A Document Driven Approach to Certifying Scientific Computing Software*. McMaster University, Hamilton, ON, Canada: 2013. Print.

<div id="parnasClements1986"></div>

[3]: Parnas, David L. and Clements, P. C. "A rational design process: How and why to fake it." *IEEE Transactions on Software Engineering*, vol. 12, no. 2, Washington, USA: February, 1986. pp. 251&ndash;257. Print.

<div id="smithKoothoor2016"></div>

[4]: Smith, W. Spencer and Koothoor, Nirmitha. "A Document-Driven Method for Certifying Scientific Computing Software for Use in Nuclear Safety Analysis." * Nuclear Engineering and Technology*, vol. 48, no. 2, April, 2016. <http://www.sciencedirect.com/science/article/pii/S1738573315002582>. pp. 404&ndash;418.

<div id="smithLai2005"></div>

[5]: Smith, W. Spencer and Lai, Lei. "A new requirements template for scientific computing." *Proceedings of the First International Workshop on Situational Requirements Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements Engineering Processes, SREP'05*. Edited by PJ Agerfalk, N. Kraiem, and J. Ralyte, Paris, France: 2005. pp. 107&ndash;121. In conjunction with 13th IEEE International Requirements Engineering Conference,

<div id="smithEtAl2007"></div>

[6]: Smith, W. Spencer, Lai, Lei, and Khedri, Ridha. "Requirements Analysis for Engineering Computation: A Systematic Approach for Improving Software Reliability." *Reliable Computing, Special Issue on Reliable Engineering Computation*, vol. 13, no. 1, February, 2007. <https://doi.org/10.1007/s11155-006-9020-7>. pp. 83&ndash;107.

<div id="accelerationWiki"></div>

[7]: Wikipedia Contributors. *Acceleration*. June, 2019. <https://en.wikipedia.org/wiki/Acceleration>.

<div id="cartesianWiki"></div>

[8]: Wikipedia Contributors. *Cartesian coordinate system*. June, 2019. <https://en.wikipedia.org/wiki/Cartesian_coordinate_system>.

<div id="velocityWiki"></div>

[9]: Wikipedia Contributors. *Velocity*. June, 2019. <https://en.wikipedia.org/wiki/Velocity>.

