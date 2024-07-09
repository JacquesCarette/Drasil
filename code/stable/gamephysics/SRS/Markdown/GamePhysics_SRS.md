# Software Requirements Specification for GamePhysics

Alex Halliwushka, Luthfi Mawarid, and Olu Owojaiye

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
- [Off-The-Shelf Solutions](#Sec:offShelfSolns)
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
|\\({\text{J}}\\)  |energy     |joule   |
|\\({\text{kg}}\\) |mass       |kilogram|
|\\({\text{m}}\\)  |length     |metre   |
|\\({\text{N}}\\)  |force      |newton  |
|\\({\text{rad}}\\)|angle      |radian  |
|\\({\text{s}}\\)  |time       |second  |

**<p align="center">Table of Units</p>**

# Table of Symbols {#Sec:ToS}

The symbols used in this document are summarized in the [Table of Symbols](#Table:ToS) along with their units. Throughout the document, symbols in bold will represent vectors, and scalars otherwise. The symbols are listed in alphabetical order. For vector quantities, the units shown are for each component of the vector.

<div id="Table:ToS"></div>

|Symbol                                                                  |Description                                                                             |Units                                           |
|:-----------------------------------------------------------------------|:---------------------------------------------------------------------------------------|:-----------------------------------------------|
|\\(a\text{(}t\text{)}\\)                                                |Linear acceleration                                                                     |\\(\frac{\text{m}}{\text{s}^{2}}\\)             |
|\\(\boldsymbol{a}\text{(}t\text{)}\\)                                   |Acceleration                                                                            |\\(\frac{\text{m}}{\text{s}^{2}}\\)             |
|\\({\boldsymbol{a}\text{(}t\text{)}\_{j}}\\)                            |J-Th Body's Acceleration                                                                |\\(\frac{\text{m}}{\text{s}^{2}}\\)             |
|\\({C\_{\text{R}}}\\)                                                   |Coefficient of restitution                                                              |--                                              |
|\\({d\_{j}}\\)                                                          |Distance Between the J-Th Particle and the Axis of Rotation                             |\\({\text{m}}\\)                                |
|\\(\boldsymbol{d}\\)                                                    |Distance between the center of mass of the rigid bodies                                 |\\({\text{m}}\\)                                |
|\\(\boldsymbol{\hat{d}}\\)                                              |Unit vector directed from the center of the large mass to the center of the smaller mass|\\({\text{m}}\\)                                |
|\\(\|\boldsymbol{d}\|\\)                                                |Euclidean norm of the distance between the center of mass of two bodies                 |\\({\text{m}}\\)                                |
|\\({\|\boldsymbol{d}\|^{2}}\\)                                          |Squared distance                                                                        |\\({\text{m}^{2}}\\)                            |
|\\(\boldsymbol{F}\\)                                                    |Force                                                                                   |\\({\text{N}}\\)                                |
|\\({\boldsymbol{F}\_{1}}\\)                                             |Force exerted by the first body (on another body)                                       |\\({\text{N}}\\)                                |
|\\({\boldsymbol{F}\_{2}}\\)                                             |Force exerted by the second body (on another body)                                      |\\({\text{N}}\\)                                |
|\\({\boldsymbol{F}\_{\boldsymbol{g}}}\\)                                |Force of gravity                                                                        |\\({\text{N}}\\)                                |
|\\({\boldsymbol{F}\_{j}}\\)                                             |Force Applied to the J-Th Body at Time T                                                |\\({\text{N}}\\)                                |
|\\(G\\)                                                                 |Gravitational constant                                                                  |\\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\)|
|\\(\boldsymbol{g}\\)                                                    |Gravitational acceleration                                                              |\\(\frac{\text{m}}{\text{s}^{2}}\\)             |
|\\(h\\)                                                                 |Height                                                                                  |\\({\text{m}}\\)                                |
|\\(\boldsymbol{I}\\)                                                    |Moment of inertia                                                                       |\\(\text{kg}\text{m}^{2}\\)                     |
|\\({\boldsymbol{I}\_{\text{A}}}\\)                                      |Moment of Inertia of Rigid Body A                                                       |\\(\text{kg}\text{m}^{2}\\)                     |
|\\({\boldsymbol{I}\_{\text{B}}}\\)                                      |Moment of Inertia of Rigid Body B                                                       |\\(\text{kg}\text{m}^{2}\\)                     |
|\\(\boldsymbol{J}\\)                                                    |Impulse (vector)                                                                        |\\(\text{N}\text{s}\\)                          |
|\\(j\\)                                                                 |Impulse (scalar)                                                                        |\\(\text{N}\text{s}\\)                          |
|\\(KE\\)                                                                |Kinetic energy                                                                          |\\({\text{J}}\\)                                |
|\\(L\\)                                                                 |Length                                                                                  |\\({\text{m}}\\)                                |
|\\(M\\)                                                                 |Mass of the Larger Rigid Body                                                           |\\({\text{kg}}\\)                               |
|\\(m\\)                                                                 |Mass                                                                                    |\\({\text{kg}}\\)                               |
|\\({m\_{1}}\\)                                                          |Mass of the first body                                                                  |\\({\text{kg}}\\)                               |
|\\({m\_{2}}\\)                                                          |Mass of the second body                                                                 |\\({\text{kg}}\\)                               |
|\\({m\_{\text{A}}}\\)                                                   |Mass of Rigid Body A                                                                    |\\({\text{kg}}\\)                               |
|\\({m\_{\text{B}}}\\)                                                   |Mass of Rigid Body B                                                                    |\\({\text{kg}}\\)                               |
|\\({m\_{j}}\\)                                                          |Mass of the J-Th Particle                                                               |\\({\text{kg}}\\)                               |
|\\({m\_{T}}\\)                                                          |Total Mass of the Rigid Body                                                            |\\({\text{kg}}\\)                               |
|\\(\boldsymbol{n}\\)                                                    |Collision normal vector                                                                 |\\({\text{m}}\\)                                |
|\\(\|\boldsymbol{n}\|\\)                                                |Length of the normal vector                                                             |\\({\text{m}}\\)                                |
|\\(PE\\)                                                                |Potential energy                                                                        |\\({\text{J}}\\)                                |
|\\(\boldsymbol{p}\text{(}t\text{)}\\)                                   |Position                                                                                |\\({\text{m}}\\)                                |
|\\({\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}\\)                    |Center of Mass                                                                          |\\({\text{m}}\\)                                |
|\\({\boldsymbol{p}\text{(}t\text{)}\_{j}}\\)                            |Position Vector of the J-Th Particle                                                    |\\({\text{m}}\\)                                |
|\\(\boldsymbol{r}\\)                                                    |Position vector                                                                         |\\({\text{m}}\\)                                |
|\\(t\\)                                                                 |Time                                                                                    |\\({\text{s}}\\)                                |
|\\({t\_{\text{c}}}\\)                                                   |Denotes the time at collision                                                           |\\({\text{s}}\\)                                |
|\\(u\text{(}t\text{)}\\)                                                |Linear displacement                                                                     |\\({\text{m}}\\)                                |
|\\(\boldsymbol{u}\\)                                                    |Displacement                                                                            |\\({\text{m}}\\)                                |
|\\(\|{\boldsymbol{u}\_{\text{A}\text{P}}}\text{\*}\boldsymbol{n}\|\\)   |Length of the Perpendicular Vector to the Contact Displacement Vector of Rigid Body A   |\\({\text{m}}\\)                                |
|\\(\|{\boldsymbol{u}\_{\text{B}\text{P}}}\text{\*}\boldsymbol{n}\|\\)   |Length of the Perpendicular Vector to the Contact Displacement Vector of Rigid Body B   |\\({\text{m}}\\)                                |
|\\({\boldsymbol{u}\_{\text{O}\text{B}}}\\)                              |Displacement vector between the origin and point B                                      |\\({\text{m}}\\)                                |
|\\(v\text{(}t\text{)}\\)                                                |Linear velocity                                                                         |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\(\boldsymbol{v}\text{(}t\text{)}\\)                                   |Velocity                                                                                |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\(Δ\boldsymbol{v}\\)                                                   |Change in velocity                                                                      |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}^{\text{A}\text{P}}}\\)              |Velocity of the Point of Collision P in Body A                                          |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}^{\text{B}\text{P}}}\\)              |Velocity of the Point of Collision P in Body B                                          |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{1}}\\)                            |Velocity of the First Body                                                              |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{2}}\\)                            |Velocity of the Second Body                                                             |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{A}}}\\)                     |Velocity at Point A                                                                     |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\)                     |Velocity at Point B                                                                     |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{f}}}^{\text{A}\text{B}}}\\)|Final Relative Velocity Between Rigid Bodies of A and B                                 |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\)|Initial Relative Velocity Between Rigid Bodies of A and B                               |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{j}}\\)                            |Velocity of the J-Th Body                                                               |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}\\)                     |Velocity at Point Origin                                                                |\\(\frac{\text{m}}{\text{s}}\\)                 |
|\\(α\\)                                                                 |Angular acceleration                                                                    |\\(\frac{\text{rad}}{\text{s}^{2}}\\)           |
|\\({α\_{j}}\\)                                                          |J-Th Body's Angular Acceleration                                                        |\\(\frac{\text{rad}}{\text{s}^{2}}\\)           |
|\\(θ\\)                                                                 |Angular displacement                                                                    |\\({\text{rad}}\\)                              |
|\\(\boldsymbol{τ}\\)                                                    |Torque                                                                                  |\\(\text{N}\text{m}\\)                          |
|\\({\boldsymbol{τ}\_{j}}\\)                                             |Torque applied to the j-th body                                                         |\\(\text{N}\text{m}\\)                          |
|\\(ω\\)                                                                 |Angular velocity                                                                        |\\(\frac{\text{rad}}{\text{s}}\\)               |
|\\(ϕ\\)                                                                 |Orientation                                                                             |\\({\text{rad}}\\)                              |

**<p align="center">Table of Symbols</p>**

# Abbreviations and Acronyms {#Sec:TAbbAcc}

<div id="Table:TAbbAcc"></div>

|Abbreviation|Full Form                          |
|:-----------|:----------------------------------|
|2D          |Two-Dimensional                    |
|3D          |Three-Dimensional                  |
|A           |Assumption                         |
|CM          |Centre of Mass                     |
|DD          |Data Definition                    |
|GD          |General Definition                 |
|GS          |Goal Statement                     |
|IM          |Instance Model                     |
|LC          |Likely Change                      |
|ODE         |Ordinary Differential Equation     |
|R           |Requirement                        |
|RefBy       |Referenced by                      |
|Refname     |Reference Name                     |
|SRS         |Software Requirements Specification|
|TM          |Theoretical Model                  |
|UC          |Unlikely Change                    |
|Uncert.     |Typical Uncertainty                |

**<p align="center">Abbreviations and Acronyms</p>**

# Introduction {#Sec:Intro}

Due to the rising cost of developing video games, developers are looking for ways to save time and money for their projects. Using an open source physics library that is reliable and free will cut down development costs and lead to better quality products.

The following section provides an overview of the Software Requirements Specification (SRS) for GamePhysics. This section explains the purpose of this document, the scope of the requirements, the characteristics of the intended reader, and the organization of the document.

# Purpose of Document {#Sec:DocPurpose}

The primary purpose of this document is to record the requirements of GamePhysics. Goals, assumptions, theoretical models, definitions, and other model derivation information are specified, allowing the reader to fully understand and verify the purpose and scientific basis of GamePhysics. With the exception of [system constraints](#Sec:SysConstraints), this SRS will remain abstract, describing what problem is being solved, but not how to solve it.

This document will be used as a starting point for subsequent development phases, including writing the design specification and the software verification and validation plan. The design document will show how the requirements are to be realized, including decisions on the numerical algorithms and programming environment. The verification and validation plan will show the steps that will be used to increase confidence in the software documentation and the implementation. Although the SRS fits in a series of documents that follow the so-called waterfall model, the actual development process is not constrained in any way. Even when the waterfall model is not followed, as Parnas and Clements point out [parnasClements1986](#parnasClements1986), the most logical way to present the documentation is still to "fake" a rational design process.

# Scope of Requirements {#Sec:ReqsScope}

The scope of the requirements includes the physical simulation of 2D rigid bodies acted on by forces.

# Characteristics of Intended Reader {#Sec:ReaderChars}

Reviewers of this documentation should have an understanding of rigid body dynamics and high school calculus. The users of GamePhysics can have a lower level of expertise, as explained in [Sec:User Characteristics](#Sec:UserChars).

# Organization of Document {#Sec:DocOrg}

The organization of this document follows the template for an SRS for scientific computing software proposed by [koothoor2013](#koothoor2013), [smithLai2005](#smithLai2005), [smithEtAl2007](#smithEtAl2007), and [smithKoothoor2016](#smithKoothoor2016). The presentation follows the standard pattern of presenting goals, theories, definitions, and assumptions. For readers that would like a more bottom up approach, they can start reading the [instance models](#Sec:IMs) and trace back to find any additional information they require.

The [goal statements](#Sec:GoalStmt) are refined to the theoretical models and the [theoretical models](#Sec:TMs) to the [instance models](#Sec:IMs).

# General System Description {#Sec:GenSysDesc}

This section provides general information about the system. It identifies the interfaces between the system and its environment, describes the user characteristics, and lists the system constraints.

# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](#Figure:sysCtxDiag) shows the system context. A circle represents an entity external to the software, the user in this case. A rectangle represents the software system itself (GamePhysics). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag"></div>

![System Context](/assets/sysctx.png)

**<p align="center">System Context</p>**

The interaction between the product and the user is through an application programming interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide initial conditions of the physical state of the simulation, rigid bodies present, and forces applied to them.
  - Ensure application programming interface use complies with the user guide.
  - Ensure required [software assumptions](#Sec:Assumps) are appropriate for any particular problem the software addresses.
- GamePhysics Responsibilities
  - Determine if the inputs and simulation state satisfy the required [physical and system constraints](#Sec:DataConstraints).
  - Calculate the new state of all rigid bodies within the simulation at each simulation step.
  - Provide updated physical state of all rigid bodies at the end of a simulation step.

# User Characteristics {#Sec:UserChars}

The end user of GamePhysics should have an understanding of first year programming concepts and an understanding of high school physics.

# System Constraints {#Sec:SysConstraints}

There are no system constraints.

# Specific System Description {#Sec:SpecSystDesc}

This section first presents the problem description, which gives a high-level view of the problem to be solved. This is followed by the solution characteristics specification, which presents the assumptions, theories, and definitions that are used.

# Problem Description {#Sec:ProbDesc}

A system is needed to simulate 2D rigid body physics for use in game development in a simple, lightweight, fast, and portable manner, which will allow for the production of higher quality products. Creating a gaming physics library is a difficult task. Games need physics libraries that simulate objects acting under various physical conditions, while simultaneously being fast and efficient enough to work in soft real-time during the game. Developing a physics library from scratch takes a long period of time and is very costly, presenting barriers of entry which make it difficult for game developers to include physics in their products. There are a few free, open source and high quality [physics libraries](#Sec:offShelfSolns) available to be used for consumer products.

# Terminology and Definitions {#Sec:TermDefs}

This subsection provides a list of terms that are used in the subsequent sections and their meaning, with the purpose of reducing ambiguity and making it easier to correctly understand the requirements.

- Rigid body: A solid body in which deformation is neglected.
- Elasticity: The ratio of the relative velocities of two colliding objects after and before a collision.
- Centre of mass: The mean location of the distribution of mass of the object.
- Cartesian coordinate system: A coordinate system that specifies each point uniquely in a plane by a set of numerical coordinates, which are the signed distances to the point from two fixed perpendicular oriented lines, measured in the same unit of length (from [cartesianWiki](#cartesianWiki)).
- Right-handed coordinate system: A coordinate system where the positive z-axis comes out of the screen..
- line: An interval between two points (from [lineSource](#lineSource)).
- point: An exact location, it has no size, only position (from [pointSource](#pointSource)).
- damping: An influence within or upon an oscillatory system that has the effect of reducing, restricting or preventing its oscillations (from [dampingSource](#dampingSource)).

# Goal Statements {#Sec:GoalStmt}

Given the kinematic properties, and forces (including any collision forces) applied on a set of rigid bodies, the goal statements are:

<div id="linearGS"></div>

Determine-Linear-Properties: Determine their new positions and velocities over a period of time.

<div id="angularGS"></div>

Determine-Angular-Properties: Determine their new orientations and angular velocities over a period of time.

# Solution Characteristics Specification {#Sec:SolCharSpec}

The instance models that govern GamePhysics are presented in the [Instance Model Section](#Sec:IMs). The information to understand the meaning of the instance models and their derivation is also presented, so that the instance models can be verified.

# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="assumpOT"></div>

objectTy: All objects are rigid bodies. (RefBy: [GD:impulse](#GD:impulse), [IM:rotMot](#IM:rotMot), [IM:transMot](#IM:transMot), [DD:chaslesThm](#DD:chaslesThm), [DD:reVeInColl](#DD:reVeInColl), [DD:potEnergy](#DD:potEnergy), [DD:ctrOfMass](#DD:ctrOfMass), [DD:momentOfInertia](#DD:momentOfInertia), [DD:linVel](#DD:linVel), [DD:linDisp](#DD:linDisp), [DD:linAcc](#DD:linAcc), [DD:kEnergy](#DD:kEnergy), [DD:impulseV](#DD:impulseV), [IM:col2D](#IM:col2D), [DD:angVel](#DD:angVel), [DD:angDisp](#DD:angDisp), and [DD:angAccel](#DD:angAccel).)

<div id="assumpOD"></div>

objectDimension: All objects are 2D. (RefBy: [GD:impulse](#GD:impulse), [IM:rotMot](#IM:rotMot), [IM:transMot](#IM:transMot), [DD:potEnergy](#DD:potEnergy), [TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot), [DD:kEnergy](#DD:kEnergy), [IM:col2D](#IM:col2D), [DD:angVel](#DD:angVel), [DD:angDisp](#DD:angDisp), and [DD:angAccel](#DD:angAccel).)

<div id="assumpCST"></div>

coordinateSystemTy: The library uses a Cartesian coordinate system.

<div id="assumpAD"></div>

axesDefined: The axes are defined using right-handed coordinate system. (RefBy: [GD:impulse](#GD:impulse), [IM:rotMot](#IM:rotMot), and [IM:col2D](#IM:col2D).)

<div id="assumpCT"></div>

collisionType: All rigid bodies collisions are vertex-to-edge collisions. (RefBy: [GD:impulse](#GD:impulse), [LC:Expanded-Collisions](#lcEC), and [IM:col2D](#IM:col2D).)

<div id="assumpDI"></div>

dampingInvolvement: There is no damping involved throughout the simulation and this implies that there are no friction forces. (RefBy: [IM:transMot](#IM:transMot), [DD:potEnergy](#DD:potEnergy), [LC:Include-Dampening](#lcID), [DD:kEnergy](#DD:kEnergy), and [IM:col2D](#IM:col2D).)

<div id="assumpCAJI"></div>

constraintsAndJointsInvolvement: There are no constraints and joints involved throughout the simulation. (RefBy: [IM:transMot](#IM:transMot), [LC:Include-Joints-Constraints](#lcIJC), and [IM:col2D](#IM:col2D).)

# Theoretical Models {#Sec:TMs}

This section focuses on the general equations and laws that GamePhysics is based on.

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
|RefBy      |[IM:transMot](#IM:transMot)                                                                                                                                                                                                         |

<div align="center">

## Newton's third law of motion {#TM:NewtonThirdLawMot}

</div>

|Refname    |TM:NewtonThirdLawMot                                                                                                                                                                                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's third law of motion                                                                                                                                                                                                                                                                                      |
|Equation   |\\[{\boldsymbol{F}\_{1}}=-{\boldsymbol{F}\_{2}}\\]                                                                                                                                                                                                                                                                |
|Description|<ul><li>\\({\boldsymbol{F}\_{1}}\\) is the force exerted by the first body (on another body) (\\({\text{N}}\\))</li><li>\\({\boldsymbol{F}\_{2}}\\) is the force exerted by the second body (on another body) (\\({\text{N}}\\))</li></ul>                                                                        |
|Notes      |<ul><li>Every action has an equal and opposite reaction. In other words, the force \\({\boldsymbol{F}\_{1}}\\) exerted on the second rigid body by the first is equal in magnitude and in the opposite direction to the force \\({\boldsymbol{F}\_{2}}\\) exerted on the first rigid body by the second.</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                |
|RefBy      |                                                                                                                                                                                                                                                                                                                  |

<div align="center">

## Newton's law of universal gravitation {#TM:UniversalGravLaw}

</div>

|Refname    |TM:UniversalGravLaw                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's law of universal gravitation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Equation   |\\[\boldsymbol{F}=G \frac{{m\_{1}} {m\_{2}}}{\|\boldsymbol{d}\|^{2}} \boldsymbol{\hat{d}}=G \frac{{m\_{1}} {m\_{2}}}{\|\boldsymbol{d}\|^{2}} \frac{\boldsymbol{d}}{\|\boldsymbol{d}\|}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li><li>\\(G\\) is the gravitational constant (\\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\))</li><li>\\({m\_{1}}\\) is the mass of the first body (\\({\text{kg}}\\))</li><li>\\({m\_{2}}\\) is the mass of the second body (\\({\text{kg}}\\))</li><li>\\(\|\boldsymbol{d}\|\\) is the Euclidean norm of the distance between the center of mass of two bodies (\\({\text{m}}\\))</li><li>\\(\boldsymbol{\hat{d}}\\) is the unit vector directed from the center of the large mass to the center of the smaller mass (\\({\text{m}}\\))</li><li>\\(\boldsymbol{d}\\) is the distance between the center of mass of the rigid bodies (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>Two rigid bodies in the universe attract each other with a force \\(\boldsymbol{F}\\) that is directly proportional to the product of their masses, \\({m\_{1}}\\) and \\({m\_{2}}\\), and inversely proportional to the squared distance \\({\|\boldsymbol{d}\|^{2}}\\) between them.</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|RefBy      |[GD:accelGravity](#GD:accelGravity)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |

<div align="center">

## Newton's second law for rotational motion {#TM:NewtonSecLawRotMot}

</div>

|Refname    |TM:NewtonSecLawRotMot                                                                                                                                                                                                                                                                                                                                     |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Newton's second law for rotational motion                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[\boldsymbol{τ}=\boldsymbol{I} α\\]                                                                                                                                                                                                                                                                                                                     |
|Description|<ul><li>\\(\boldsymbol{τ}\\) is the torque (\\(\text{N}\text{m}\\))</li><li>\\(\boldsymbol{I}\\) is the moment of inertia (\\(\text{kg}\text{m}^{2}\\))</li><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li></ul>                                                                                                     |
|Notes      |<ul><li>The net torque \\(\boldsymbol{τ}\\) on a rigid body is proportional to its angular acceleration \\(α\\), where \\(\boldsymbol{I}\\) denotes the moment of inertia of the rigid body as the constant of proportionality.</li><li>We also assume that all rigid bodies involved are two-dimensional (from [A:objectDimension](#assumpOD)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                        |
|RefBy      |[IM:rotMot](#IM:rotMot)                                                                                                                                                                                                                                                                                                                                   |

# General Definitions {#Sec:GDs}

This section collects the laws and equations that will be used to build the instance models.

<div align="center">

## Acceleration due to gravity {#GD:accelGravity}

</div>

|Refname    |GD:accelGravity                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Acceleration due to gravity                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Equation   |\\[\boldsymbol{g}=-\frac{G M}{\|\boldsymbol{d}\|^{2}} \boldsymbol{\hat{d}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(G\\) is the gravitational constant (\\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\))</li><li>\\(M\\) is the mass of the larger rigid body (\\({\text{kg}}\\))</li><li>\\(\|\boldsymbol{d}\|\\) is the Euclidean norm of the distance between the center of mass of two bodies (\\({\text{m}}\\))</li><li>\\(\boldsymbol{\hat{d}}\\) is the unit vector directed from the center of the large mass to the center of the smaller mass (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>If one of the masses is much larger than the other, it is convenient to define a gravitational field around the larger mass as shown above. The negative sign in the equation indicates that the force is an attractive force.</li></ul>                                                                                                                                                                                                                                                                                                                                 |
|Source     |[Definition of Gravitational Acceleration](https://en.wikipedia.org/wiki/Gravitational_acceleration)                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|RefBy      |[IM:transMot](#IM:transMot)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

#### Detailed derivation of gravitational acceleration: {#GD:accelGravityDeriv}

From [Newton's law of universal gravitation](#TM:UniversalGravLaw), we have:

\\[\boldsymbol{F}=\frac{G {m\_{1}} {m\_{2}}}{{\|\boldsymbol{d}\|^{2}}} \boldsymbol{\hat{d}}\\]

The above equation governs the gravitational attraction between two bodies. Suppose that one of the bodies is significantly more massive than the other, so that we concern ourselves with the force the massive body exerts on the lighter body. Further, suppose that the Cartesian coordinate system is chosen such that this force acts on a line which lies along one of the principal axes. Then our unit vector directed from the center of the large mass to the center of the smaller mass \\(\boldsymbol{\hat{d}}\\) for the x or y axes is:

\\[\boldsymbol{\hat{d}}=\frac{\boldsymbol{d}}{\|\boldsymbol{d}\|}\\]

Given the above assumptions, let \\(M\\) and \\(m\\) be the mass of the massive and light body respectively. Equating \\(\boldsymbol{F}\\) above with Newton's second law for the force experienced by the light body, we get:

\\[{\boldsymbol{F}\_{\boldsymbol{g}}}=G \frac{M m}{{\|\boldsymbol{d}\|^{2}}} \boldsymbol{\hat{d}}=m \boldsymbol{g}\\]

where \\(\boldsymbol{g}\\) is the gravitational acceleration. Dividing the above equation by \\(m\\),  we have:

\\[G \frac{M}{{\|\boldsymbol{d}\|^{2}}} \boldsymbol{\hat{d}}=\boldsymbol{g}\\]

and thus the negative sign indicates that the force is an attractive force:

\\[\boldsymbol{g}=-G \frac{M}{{\|\boldsymbol{d}\|^{2}}} \boldsymbol{\hat{d}}\\]

<div align="center">

## Impulse for Collision {#GD:impulse}

</div>

|Refname    |GD:impulse                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Impulse for Collision                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Units      |\\(\text{N}\text{s}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Equation   |\\[j=\frac{-\left(1+{C\_{\text{R}}}\right) {{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}{\left(\frac{1}{{m\_{\text{A}}}}+\frac{1}{{m\_{\text{B}}}}\right) \|\boldsymbol{n}\|^{2}+\frac{\|{\boldsymbol{u}\_{\text{A}\text{P}}}\text{\*}\boldsymbol{n}\|^{2}}{{\boldsymbol{I}\_{\text{A}}}}+\frac{\|{\boldsymbol{u}\_{\text{B}\text{P}}}\text{\*}\boldsymbol{n}\|^{2}}{{\boldsymbol{I}\_{\text{B}}}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Description|<ul><li>\\(j\\) is the impulse (scalar) (\\(\text{N}\text{s}\\))</li><li>\\({C\_{\text{R}}}\\) is the coefficient of restitution (Unitless)</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(\boldsymbol{n}\\) is the collision normal vector (\\({\text{m}}\\))</li><li>\\({m\_{\text{A}}}\\) is the mass of rigid body A (\\({\text{kg}}\\))</li><li>\\({m\_{\text{B}}}\\) is the mass of rigid body B (\\({\text{kg}}\\))</li><li>\\(\|\boldsymbol{n}\|\\) is the length of the normal vector (\\({\text{m}}\\))</li><li>\\(\|{\boldsymbol{u}\_{\text{A}\text{P}}}\text{\*}\boldsymbol{n}\|\\) is the length of the perpendicular vector to the contact displacement vector of rigid body A (\\({\text{m}}\\))</li><li>\\({\boldsymbol{I}\_{\text{A}}}\\) is the moment of inertia of rigid body A (\\(\text{kg}\text{m}^{2}\\))</li><li>\\(\|{\boldsymbol{u}\_{\text{B}\text{P}}}\text{\*}\boldsymbol{n}\|\\) is the length of the perpendicular vector to the contact displacement vector of rigid body B (\\({\text{m}}\\))</li><li>\\({\boldsymbol{I}\_{\text{B}}}\\) is the moment of inertia of rigid body B (\\(\text{kg}\text{m}^{2}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>A right-handed coordinate system is used (from [A:axesDefined](#assumpAD)).</li><li>All collisions are vertex-to-edge (from [A:collisionType](#assumpCT)).</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Source     |[Impulse for Collision Ref](http://www.chrishecker.com/images/e/e7/Gdmphys3.pdf)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|RefBy      |[IM:col2D](#IM:col2D)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

# Data Definitions {#Sec:DDs}

This section collects and defines all the data needed to build the instance models.

<div align="center">

## Center of Mass {#DD:ctrOfMass}

</div>

|Refname    |DD:ctrOfMass                                                                                                                                                                                                                                                                                                                                                                                  |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Center of Mass                                                                                                                                                                                                                                                                                                                                                                                |
|Symbol     |\\({\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}\\)                                                                                                                                                                                                                                                                                                                                          |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                                                                                                                                                                                              |
|Equation   |\\[{\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}=\frac{\displaystyle\sum{{m\_{j}} {\boldsymbol{p}\text{(}t\text{)}\_{j}}}}{{m\_{T}}}\\]                                                                                                                                                                                                                                                      |
|Description|<ul><li>\\({\boldsymbol{p}\text{(}t\text{)}\_{\text{CM}}}\\) is the Center of Mass (\\({\text{m}}\\))</li><li>\\({m\_{j}}\\) is the mass of the j-th particle (\\({\text{kg}}\\))</li><li>\\({\boldsymbol{p}\text{(}t\text{)}\_{j}}\\) is the position vector of the j-th particle (\\({\text{m}}\\))</li><li>\\({m\_{T}}\\) is the total mass of the rigid body (\\({\text{kg}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                                                                                                                                                                                                                                                                                                          |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                            |
|RefBy      |[IM:transMot](#IM:transMot) and [IM:col2D](#IM:col2D)                                                                                                                                                                                                                                                                                                                                         |

<div align="center">

## Linear displacement {#DD:linDisp}

</div>

|Refname    |DD:linDisp                                                                                                                                                                                                              |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear displacement                                                                                                                                                                                                     |
|Symbol     |\\(u\text{(}t\text{)}\\)                                                                                                                                                                                                |
|Units      |\\({\text{m}}\\)                                                                                                                                                                                                        |
|Equation   |\\[u\text{(}t\text{)}=\frac{\\,d\boldsymbol{p}\text{(}t\text{)}\left(t\right)}{\\,dt}\\]                                                                                                                                |
|Description|<ul><li>\\(u\text{(}t\text{)}\\) is the linear displacement (\\({\text{m}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                                                                                                                                    |
|Source     |--                                                                                                                                                                                                                      |
|RefBy      |[IM:transMot](#IM:transMot)                                                                                                                                                                                             |

<div align="center">

## Linear velocity {#DD:linVel}

</div>

|Refname    |DD:linVel                                                                                                                                                                                                             |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear velocity                                                                                                                                                                                                       |
|Symbol     |\\(v\text{(}t\text{)}\\)                                                                                                                                                                                              |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                       |
|Equation   |\\[v\text{(}t\text{)}=\frac{\\,d\boldsymbol{u}\left(t\right)}{\\,dt}\\]                                                                                                                                               |
|Description|<ul><li>\\(v\text{(}t\text{)}\\) is the linear velocity (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{u}\\) is the displacement (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                                                                                                                                  |
|Source     |--                                                                                                                                                                                                                    |
|RefBy      |[IM:transMot](#IM:transMot)                                                                                                                                                                                           |

<div align="center">

## Linear acceleration {#DD:linAcc}

</div>

|Refname    |DD:linAcc                                                                                                                                                                                                                                                 |
|:----------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Linear acceleration                                                                                                                                                                                                                                       |
|Symbol     |\\(a\text{(}t\text{)}\\)                                                                                                                                                                                                                                  |
|Units      |\\(\frac{\text{m}}{\text{s}^{2}}\\)                                                                                                                                                                                                                       |
|Equation   |\\[a\text{(}t\text{)}=\frac{\\,d\boldsymbol{v}\text{(}t\text{)}\left(t\right)}{\\,dt}\\]                                                                                                                                                                  |
|Description|<ul><li>\\(a\text{(}t\text{)}\\) is the linear acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                                                                                                                                                                      |
|Source     |--                                                                                                                                                                                                                                                        |
|RefBy      |[IM:transMot](#IM:transMot)                                                                                                                                                                                                                               |

<div align="center">

## Angular displacement {#DD:angDisp}

</div>

|Refname    |DD:angDisp                                                                                                                                                                       |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular displacement                                                                                                                                                             |
|Symbol     |\\(θ\\)                                                                                                                                                                          |
|Units      |\\({\text{rad}}\\)                                                                                                                                                               |
|Equation   |\\[θ=\frac{\\,dϕ\left(t\right)}{\\,dt}\\]                                                                                                                                        |
|Description|<ul><li>\\(θ\\) is the angular displacement (\\({\text{rad}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(ϕ\\) is the orientation (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li></ul>                                   |
|Source     |--                                                                                                                                                                               |
|RefBy      |[IM:rotMot](#IM:rotMot)                                                                                                                                                          |

<div align="center">

## Angular velocity {#DD:angVel}

</div>

|Refname    |DD:angVel                                                                                                                                                                                            |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular velocity                                                                                                                                                                                     |
|Symbol     |\\(ω\\)                                                                                                                                                                                              |
|Units      |\\(\frac{\text{rad}}{\text{s}}\\)                                                                                                                                                                    |
|Equation   |\\[ω=\frac{\\,dθ\left(t\right)}{\\,dt}\\]                                                                                                                                                            |
|Description|<ul><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(θ\\) is the angular displacement (\\({\text{rad}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li></ul>                                                       |
|Source     |--                                                                                                                                                                                                   |
|RefBy      |[IM:rotMot](#IM:rotMot)                                                                                                                                                                              |

<div align="center">

## Angular acceleration {#DD:angAccel}

</div>

|Refname    |DD:angAccel                                                                                                                                                                                                             |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Angular acceleration                                                                                                                                                                                                    |
|Symbol     |\\(α\\)                                                                                                                                                                                                                 |
|Units      |\\(\frac{\text{rad}}{\text{s}^{2}}\\)                                                                                                                                                                                   |
|Equation   |\\[α=\frac{\\,dω\left(t\right)}{\\,dt}\\]                                                                                                                                                                               |
|Description|<ul><li>\\(α\\) is the angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li></ul>                                                                          |
|Source     |--                                                                                                                                                                                                                      |
|RefBy      |[IM:rotMot](#IM:rotMot)                                                                                                                                                                                                 |

<div align="center">

## Chasles' theorem {#DD:chaslesThm}

</div>

|Refname    |DD:chaslesThm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Chasles' theorem                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Symbol     |\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}={\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}+ω\times{\boldsymbol{u}\_{\text{O}\text{B}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                   |
|Description|<ul><li>\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\) is the velocity at point B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}\\) is the velocity at point origin (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(ω\\) is the angular velocity (\\(\frac{\text{rad}}{\text{s}}\\))</li><li>\\({\boldsymbol{u}\_{\text{O}\text{B}}}\\) is the displacement vector between the origin and point B (\\({\text{m}}\\))</li></ul>                                                                    |
|Notes      |<ul><li>The linear velocity \\({\boldsymbol{v}\text{(}t\text{)}\_{\text{B}}}\\) of any point B in a rigid body is the sum of the linear velocity \\({\boldsymbol{v}\text{(}t\text{)}\_{\text{O}}}\\) of the rigid body at the origin (axis of rotation) and the resultant vector from the cross product of the rigid body's angular velocity \\(ω\\) and the displacement vector between the origin and point B \\({\boldsymbol{u}\_{\text{O}\text{B}}}\\).</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>|
|Source     |[chaslesWiki](#chaslesWiki)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

<div align="center">

## Torque {#DD:torque}

</div>

|Refname    |DD:torque                                                                                                                                                                                                        |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Torque                                                                                                                                                                                                           |
|Symbol     |\\(\boldsymbol{τ}\\)                                                                                                                                                                                             |
|Units      |\\(\text{N}\text{m}\\)                                                                                                                                                                                           |
|Equation   |\\[\boldsymbol{τ}=\boldsymbol{r}\times\boldsymbol{F}\\]                                                                                                                                                          |
|Description|<ul><li>\\(\boldsymbol{τ}\\) is the torque (\\(\text{N}\text{m}\\))</li><li>\\(\boldsymbol{r}\\) is the position vector (\\({\text{m}}\\))</li><li>\\(\boldsymbol{F}\\) is the force (\\({\text{N}}\\))</li></ul>|
|Notes      |<ul><li>The torque on a body measures the tendency of a force to rotate the body around an axis or pivot.</li></ul>                                                                                              |
|Source     |--                                                                                                                                                                                                               |
|RefBy      |                                                                                                                                                                                                                 |

<div align="center">

## Kinetic energy {#DD:kEnergy}

</div>

|Refname    |DD:kEnergy                                                                                                                                                                                                                                                                                                                      |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Kinetic energy                                                                                                                                                                                                                                                                                                                  |
|Symbol     |\\(KE\\)                                                                                                                                                                                                                                                                                                                        |
|Units      |\\({\text{J}}\\)                                                                                                                                                                                                                                                                                                                |
|Equation   |\\[KE=m \frac{\|\boldsymbol{v}\text{(}t\text{)}\|^{2}}{2}\\]                                                                                                                                                                                                                                                                    |
|Description|<ul><li>\\(KE\\) is the kinetic energy (\\({\text{J}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>                                                                                                             |
|Notes      |<ul><li>Kinetic energy is the measure of the energy a body possesses due to its motion.</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>No damping occurs during the simulation (from [A:dampingInvolvement](#assumpDI)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                              |
|RefBy      |                                                                                                                                                                                                                                                                                                                                |

<div align="center">

## Coefficient of restitution {#DD:coeffRestitution}

</div>

|Refname    |DD:coeffRestitution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Coefficient of restitution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Symbol     |\\({C\_{\text{R}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Units      |Unitless                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Equation   |\\[{C\_{\text{R}}}=-\left(\frac{{{\boldsymbol{v}\text{(}t\text{)}\_{\text{f}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}{{{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\cdot{}\boldsymbol{n}}\right)\\]                                                                                                                                                                                                                                                                                                              |
|Description|<ul><li>\\({C\_{\text{R}}}\\) is the coefficient of restitution (Unitless)</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{f}}}^{\text{A}\text{B}}}\\) is the final relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\(\boldsymbol{n}\\) is the collision normal vector (\\({\text{m}}\\))</li><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>The coefficient of restitution \\({C\_{\text{R}}}\\) determines the elasticity of a collision between two rigid bodies. \\({C\_{\text{R}}}=1\\) results in an elastic collision, \\({C\_{\text{R}}}\lt{}1\\) results in an inelastic collision, and \\({C\_{\text{R}}}=0\\) results in a totally inelastic collision.</li></ul>                                                                                                                                                                                                  |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |

<div align="center">

## Initial Relative Velocity Between Rigid Bodies of A and B {#DD:reVeInColl}

</div>

|Refname    |DD:reVeInColl                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|:----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Initial Relative Velocity Between Rigid Bodies of A and B                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Symbol     |\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|Units      |\\(\frac{\text{m}}{\text{s}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Equation   |\\[{{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}={\boldsymbol{v}\text{(}t\text{)}^{\text{A}\text{P}}}-{\boldsymbol{v}\text{(}t\text{)}^{\text{B}\text{P}}}\\]                                                                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the initial relative velocity between rigid bodies of A and B (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}^{\text{A}\text{P}}}\\) is the velocity of the point of collision P in body A (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({\boldsymbol{v}\text{(}t\text{)}^{\text{B}\text{P}}}\\) is the velocity of the point of collision P in body B (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>In a collision, the velocity of a rigid body A colliding with another rigid body B relative to that body \\({{\boldsymbol{v}\text{(}t\text{)}\_{\text{i}}}^{\text{A}\text{B}}}\\) is the difference between the velocities of A and B at point P.</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                                                                                                                                                                     |
|Source     |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

<div align="center">

## Impulse (vector) {#DD:impulseV}

</div>

|Refname    |DD:impulseV                                                                                                                                                                                                                      |
|:----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Impulse (vector)                                                                                                                                                                                                                 |
|Symbol     |\\(\boldsymbol{J}\\)                                                                                                                                                                                                             |
|Units      |\\(\text{N}\text{s}\\)                                                                                                                                                                                                           |
|Equation   |\\[\boldsymbol{J}=m Δ\boldsymbol{v}\\]                                                                                                                                                                                           |
|Description|<ul><li>\\(\boldsymbol{J}\\) is the impulse (vector) (\\(\text{N}\text{s}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(Δ\boldsymbol{v}\\) is the change in velocity (\\(\frac{\text{m}}{\text{s}}\\))</li></ul>|
|Notes      |<ul><li>An impulse (vector) \\(\boldsymbol{J}\\) occurs when a force \\(\boldsymbol{F}\\) acts over a body over an interval of time.</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>        |
|Source     |--                                                                                                                                                                                                                               |
|RefBy      |                                                                                                                                                                                                                                 |

#### Detailed derivation of impulse (vector): {#DD:impulseVDeriv}

Newton's second law of motion states:

\\[\boldsymbol{F}=m \boldsymbol{a}\text{(}t\text{)}=m \frac{\\,d\boldsymbol{v}\text{(}t\text{)}}{\\,dt}\\]

Rearranging:

\\[\int\_{{t\_{1}}}^{{t\_{2}}}{\boldsymbol{F}}\\,dt=m \left(\int\_{{\boldsymbol{v}\text{(}t\text{)}\_{1}}}^{{\boldsymbol{v}\text{(}t\text{)}\_{2}}}{1}\\,d\boldsymbol{v}\text{(}t\text{)}\right)\\]

Integrating the right hand side:

\\[\int\_{{t\_{1}}}^{{t\_{2}}}{\boldsymbol{F}}\\,dt=m {\boldsymbol{v}\text{(}t\text{)}\_{2}}-m {\boldsymbol{v}\text{(}t\text{)}\_{1}}=m Δ\boldsymbol{v}\\]

<div align="center">

## Potential energy {#DD:potEnergy}

</div>

|Refname    |DD:potEnergy                                                                                                                                                                                                                                                                                                                                                |
|:----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Potential energy                                                                                                                                                                                                                                                                                                                                            |
|Symbol     |\\(PE\\)                                                                                                                                                                                                                                                                                                                                                    |
|Units      |\\({\text{J}}\\)                                                                                                                                                                                                                                                                                                                                            |
|Equation   |\\[PE=m \boldsymbol{g} h\\]                                                                                                                                                                                                                                                                                                                                 |
|Description|<ul><li>\\(PE\\) is the potential energy (\\({\text{J}}\\))</li><li>\\(m\\) is the mass (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(h\\) is the height (\\({\text{m}}\\))</li></ul>                                                                                 |
|Notes      |<ul><li>The potential energy of an object is the energy held by an object because of its position to other objects.</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>No damping occurs during the simulation (from [A:dampingInvolvement](#assumpDI)).</li></ul>|
|Source     |--                                                                                                                                                                                                                                                                                                                                                          |
|RefBy      |                                                                                                                                                                                                                                                                                                                                                            |

<div align="center">

## Moment of inertia {#DD:momentOfInertia}

</div>

|Refname    |DD:momentOfInertia                                                                                                                                                                                                                                                                    |
|:----------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label      |Moment of inertia                                                                                                                                                                                                                                                                     |
|Symbol     |\\(\boldsymbol{I}\\)                                                                                                                                                                                                                                                                  |
|Units      |\\(\text{kg}\text{m}^{2}\\)                                                                                                                                                                                                                                                           |
|Equation   |\\[\boldsymbol{I}=\displaystyle\sum{{m\_{j}} {d\_{j}}^{2}}\\]                                                                                                                                                                                                                         |
|Description|<ul><li>\\(\boldsymbol{I}\\) is the moment of inertia (\\(\text{kg}\text{m}^{2}\\))</li><li>\\({m\_{j}}\\) is the mass of the j-th particle (\\({\text{kg}}\\))</li><li>\\({d\_{j}}\\) is the distance between the j-th particle and the axis of rotation (\\({\text{m}}\\))</li></ul>|
|Notes      |<ul><li>The moment of inertia \\(\boldsymbol{I}\\) of a body measures how much torque is needed for the body to achieve angular acceleration about the axis of rotation.</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)).</li></ul>                         |
|Source     |--                                                                                                                                                                                                                                                                                    |
|RefBy      |                                                                                                                                                                                                                                                                                      |

# Instance Models {#Sec:IMs}

This section transforms the problem defined in the [problem description](#Sec:ProbDesc) into one which is expressed in mathematical terms. It uses concrete symbols defined in the [data definitions](#Sec:DDs) to replace the abstract symbols in the models identified in [theoretical models](#Sec:TMs) and [general definitions](#Sec:GDs).

The goal [GS:Determine-Linear-Properties](#linearGS) is met by [IM:transMot](#IM:transMot) and [IM:col2D](#IM:col2D). The goal [GS:Determine-Angular-Properties](#angularGS) is met by [IM:rotMot](#IM:rotMot) and [IM:col2D](#IM:col2D).

<div align="center">

## J-Th Body's Acceleration {#IM:transMot}

</div>

|Refname           |IM:transMot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:-----------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |J-Th Body's Acceleration                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Input             |\\({\boldsymbol{v}\text{(}t\text{)}\_{j}}\\), \\(t\\), \\(\boldsymbol{g}\\), \\({\boldsymbol{F}\_{j}}\\), \\({m\_{j}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Output            |\\({\boldsymbol{a}\text{(}t\text{)}\_{j}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|Input Constraints |\\[{\boldsymbol{v}\text{(}t\text{)}\_{j}}\gt{}0\\]\\[t\gt{}0\\]\\[\boldsymbol{g}\gt{}0\\]\\[{\boldsymbol{F}\_{j}}\gt{}0\\]\\[{m\_{j}}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Output Constraints|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Equation          |\\[{\boldsymbol{a}\text{(}t\text{)}\_{j}}=\boldsymbol{g}+\frac{{\boldsymbol{F}\_{j}}\left(t\right)}{{m\_{j}}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Description       |<ul><li>\\({\boldsymbol{a}\text{(}t\text{)}\_{j}}\\) is the j-th body's acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\(\boldsymbol{g}\\) is the gravitational acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{F}\_{j}}\\) is the force applied to the j-th body at time t (\\({\text{N}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\({m\_{j}}\\) is the mass of the j-th particle (\\({\text{kg}}\\))</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Notes             |<ul><li>The above equation expresses the total acceleration of the rigid body \\(j\\) as the sum of gravitational acceleration (from [GD:accelGravity](#GD:accelGravity)) and acceleration due to applied force \\({\boldsymbol{F}\_{j}}\left(t\right)\\) (from [TM:NewtonSecLawMot](#TM:NewtonSecLawMot)). The resultant outputs are then obtained from this equation using [DD:linDisp](#DD:linDisp), [DD:linVel](#DD:linVel), and [DD:linAcc](#DD:linAcc).</li><li>The output of the instance model will be the functions of position and velocity over time that satisfy the ODE for the acceleration, with the given initial conditions for position and velocity. The motion is translational, so the position and velocity functions are for the centre of mass (from [DD:ctrOfMass](#DD:ctrOfMass)).</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>It is currently assumed that no damping occurs during the simulation (from [A:dampingInvolvement](#assumpDI)) and that no constraints are involved (from [A:constraintsAndJointsInvolvement](#assumpCAJI)).</li></ul>|
|Source            |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|RefBy             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |

#### Detailed derivation of j-th body's acceleration: {#IM:transMotDeriv}

We may calculate the total acceleration of rigid body \\(j\\) by calculating the derivative of it's velocity with respect to time (from [DD:linAcc](#DD:linAcc)).

\\[{α\_{j}}=\frac{\\,d{\boldsymbol{v}\text{(}t\text{)}\_{j}}\left(t\right)}{\\,dt}\\]

Performing the derivative, we obtain:

\\[{\boldsymbol{a}\text{(}t\text{)}\_{j}}=\boldsymbol{g}+\frac{{\boldsymbol{F}\_{j}}\left(t\right)}{{m\_{j}}}\\]

<div align="center">

## J-Th Body's Angular Acceleration {#IM:rotMot}

</div>

|Refname           |IM:rotMot                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |J-Th Body's Angular Acceleration                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|Input             |\\(ω\\), \\(t\\), \\({\boldsymbol{τ}\_{j}}\\), \\(\boldsymbol{I}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|Output            |\\({α\_{j}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Input Constraints |\\[ω\gt{}0\\]\\[t\gt{}0\\]\\[{\boldsymbol{τ}\_{j}}\gt{}0\\]\\[\boldsymbol{I}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Output Constraints|\\[{α\_{j}}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|Equation          |\\[{α\_{j}}=\frac{{\boldsymbol{τ}\_{j}}\left(t\right)}{\boldsymbol{I}}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|Description       |<ul><li>\\({α\_{j}}\\) is the j-th body's angular acceleration (\\(\frac{\text{rad}}{\text{s}^{2}}\\))</li><li>\\({\boldsymbol{τ}\_{j}}\\) is the torque applied to the j-th body (\\(\text{N}\text{m}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(\boldsymbol{I}\\) is the moment of inertia (\\(\text{kg}\text{m}^{2}\\))</li></ul>                                                                                                                                                                                              |
|Notes             |<ul><li>The above equation for the total angular acceleration of the rigid body \\(j\\) is derived from [TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot), and the resultant outputs are then obtained from this equation using [DD:angDisp](#DD:angDisp), [DD:angVel](#DD:angVel), and [DD:angAccel](#DD:angAccel).</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>A right-handed coordinate system is used (from [A:axesDefined](#assumpAD)).</li></ul>|
|Source            |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|RefBy             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

#### Detailed derivation of j-th body's angular acceleration: {#IM:rotMotDeriv}

We may calculate the total angular acceleration of rigid body \\(j\\) by calculating the derivative of its angular velocity with respect to time (from [DD:angAccel](#DD:angAccel)).

\\[{α\_{j}}=\frac{\\,dω\left(t\right)}{\\,dt}\\]

Performing the derivative, we obtain:

\\[{α\_{j}}=\frac{{\boldsymbol{τ}\_{j}}\left(t\right)}{\boldsymbol{I}}\\]

<div align="center">

## Collisions on 2D rigid bodies {#IM:col2D}

</div>

|Refname           |IM:col2D                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Label             |Collisions on 2D rigid bodies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Input             |\\(t\\), \\(j\\), \\({m\_{\text{A}}}\\), \\(\boldsymbol{n}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Output            |\\({t\_{\text{c}}}\\)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Input Constraints |\\[t\gt{}0\\]\\[j\gt{}0\\]\\[{m\_{\text{A}}}\gt{}0\\]\\[\boldsymbol{n}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|Output Constraints|\\[{t\_{\text{c}}}\gt{}0\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|Equation          |\\[{\boldsymbol{v}\text{(}t\text{)}\_{\text{A}}}\left({t\_{\text{c}}}\right)={\boldsymbol{v}\text{(}t\text{)}\_{\text{A}}}\left(t\right)+\frac{j}{{m\_{\text{A}}}} \boldsymbol{n}\\]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|Description       |<ul><li>\\({\boldsymbol{v}\text{(}t\text{)}\_{\text{A}}}\\) is the velocity at point A (\\(\frac{\text{m}}{\text{s}}\\))</li><li>\\({t\_{\text{c}}}\\) is the denotes the time at collision (\\({\text{s}}\\))</li><li>\\(t\\) is the time (\\({\text{s}}\\))</li><li>\\(j\\) is the impulse (scalar) (\\(\text{N}\text{s}\\))</li><li>\\({m\_{\text{A}}}\\) is the mass of rigid body A (\\({\text{kg}}\\))</li><li>\\(\boldsymbol{n}\\) is the collision normal vector (\\({\text{m}}\\))</li></ul>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Notes             |<ul><li>The output of the instance model will be the functions of position, velocity, orientation, and angular acceleration over time that satisfy the equations for the velocity and angular acceleration, with the given initial conditions for position, velocity, orientation, and angular acceleration. The motion is translational, so the position, velocity, orientation, and angular acceleration functions are for the centre of mass (from [DD:ctrOfMass](#DD:ctrOfMass)).</li><li>All bodies are assumed to be rigid (from [A:objectTy](#assumpOT)) and two-dimensional (from [A:objectDimension](#assumpOD)).</li><li>A right-handed coordinate system is used (from [A:axesDefined](#assumpAD)).</li><li>All collisions are vertex-to-edge (from [A:collisionType](#assumpCT)).</li><li>It is currently assumed that no damping occurs during the simulation (from [A:dampingInvolvement](#assumpDI)) and that no constraints are involved (from [A:constraintsAndJointsInvolvement](#assumpCAJI)).</li><li>\\(j\\) is defined in [GD:impulse](#GD:impulse)</li></ul>|
|Source            |--                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|RefBy             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario.

<div id="Table:InDataConstraints"></div>

|Var                                  |Physical Constraints               |Software Constraints   |Typical Value                                                               |Uncert.    |
|:------------------------------------|:----------------------------------|:----------------------|:---------------------------------------------------------------------------|:----------|
|\\({C\_{\text{R}}}\\)                |\\(0\leq{}{C\_{\text{R}}}\leq{}1\\)|--                     |\\(0.8\\)                                                                   |10\\(\\%\\)|
|\\(\boldsymbol{F}\\)                 |--                                 |--                     |\\(98.1\\) \\({\text{N}}\\)                                                 |10\\(\\%\\)|
|\\(G\\)                              |--                                 |--                     |\\(66.743\cdot{}10^{-12}\\) \\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\)|10\\(\\%\\)|
|\\(\boldsymbol{I}\\)                 |\\(\boldsymbol{I}\gt{}0\\)         |--                     |\\(74.5\\) \\(\text{kg}\text{m}^{2}\\)                                      |10\\(\\%\\)|
|\\(L\\)                              |\\(L\gt{}0\\)                      |--                     |\\(44.2\\) \\({\text{m}}\\)                                                 |10\\(\\%\\)|
|\\(m\\)                              |\\(m\gt{}0\\)                      |--                     |\\(56.2\\) \\({\text{kg}}\\)                                                |10\\(\\%\\)|
|\\(\boldsymbol{p}\text{(}t\text{)}\\)|--                                 |--                     |\\(0.412\\) \\({\text{m}}\\)                                                |10\\(\\%\\)|
|\\(\boldsymbol{v}\text{(}t\text{)}\\)|--                                 |--                     |\\(2.51\\) \\(\frac{\text{m}}{\text{s}}\\)                                  |10\\(\\%\\)|
|\\(\boldsymbol{τ}\\)                 |--                                 |--                     |\\(200\\) \\(\text{N}\text{m}\\)                                            |10\\(\\%\\)|
|\\(ω\\)                              |--                                 |--                     |\\(2.1\\) \\(\frac{\text{rad}}{\text{s}}\\)                                 |10\\(\\%\\)|
|\\(ϕ\\)                              |--                                 |\\(0\leq{}ϕ\leq{}2 π\\)|\\(\frac{π}{2}\\) \\({\text{rad}}\\)                                        |10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**

# Properties of a Correct Solution {#Sec:CorSolProps}

The [Data Constraints Table](#Table:OutDataConstraints) shows the data constraints on the output variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable.

<div id="Table:OutDataConstraints"></div>

|Var                                  |
|:------------------------------------|
|\\(\boldsymbol{p}\text{(}t\text{)}\\)|
|\\(\boldsymbol{v}\text{(}t\text{)}\\)|
|\\(ϕ\\)                              |
|\\(ω\\)                              |

**<p align="center">Output Data Constraints</p>**

# Requirements {#Sec:Requirements}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete, and the non-functional requirements, the qualities that the software is expected to exhibit.

# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="simSpace"></div>

Simulation-Space: Create a space for all of the rigid bodies in the physical simulation to interact in.

<div id="inputInitialConds"></div>

Input-Initial-Conditions: Input the initial masses, velocities, orientations, angular velocities of, and forces applied on rigid bodies.

<div id="inputSurfaceProps"></div>

Input-Surface-Properties: Input the surface properties of the bodies such as friction or elasticity.

<div id="verifyPhysCons"></div>

Verify-Physical_Constraints: Verify that the inputs satisfy the required physical constraints from the [solution characteristics specification](#Sec:SolCharSpec).

<div id="calcTransOverTime"></div>

Calculate-Translation-Over-Time: Determine the positions and velocities over a period of time of the 2D rigid bodies acted upon by a force.

<div id="calcRotOverTime"></div>

Calculate-Rotation-Over-Time: Determine the orientations and angular velocities over a period of time of the 2D rigid bodies.

<div id="deterColls"></div>

Determine-Collisions: Determine if any of the rigid bodies in the space have collided.

<div id="deterCollRespOverTime"></div>

Determine-Collision-Response-Over-Time: Determine the positions and velocities over a period of time of the 2D rigid bodies that have undergone a collision.

# Non-Functional Requirements {#Sec:NFRs}

This section provides the non-functional requirements, the qualities that the software is expected to exhibit.

<div id="performance"></div>

Performance: The execution time for collision detection and collision resolution shall be comparable to an existing 2D physics library on the market (e.g. Pymunk).

<div id="correctness"></div>

Correctness: The output of simulation results shall be compared to an existing implementation like Pymunk (please refer to: http://www.pymunk.org/en/latest/).

<div id="usability"></div>

Usability: Software shall be easy to learn and use. Usability shall be measured by how long it takes a user to learn how to use the library to create a small program to simulate the movement of 2 bodies over time in space. Creating a program should take no less than 30 to 60 minutes for an intermediate to experienced programmer.

<div id="understandability"></div>

Understandability: Users of Tamias2D shall be able to learn the software with ease. Users shall be able to easily create a small program using the library. Creating a small program to simulate the movement of 2 bodies in space should take no less that 60 minutes.

<div id="maintainability"></div>

Maintainability: If a likely change is made to the finished software, it will take at most 10\\(\\%\\) of the original development time, assuming the same development resources are available.

# Likely Changes {#Sec:LCs}

This section lists the likely changes to be made to the software.

<div id="lcVODES"></div>

Variable-ODE-Solver: The internal ODE-solving algorithm used by the library may be changed in the future.

<div id="lcEC"></div>

Expanded-Collisions: [A:collisionType](#assumpCT) - The library may be expanded to deal with edge-to-edge and vertex-to-vertex collisions.

<div id="lcID"></div>

Include-Dampening: [A:dampingInvolvement](#assumpDI) - The library may be expanded to include motion with damping.

<div id="lcIJC"></div>

Include-Joints-Constraints: [A:constraintsAndJointsInvolvement](#assumpCAJI) - The library may be expanded to include joints and constraints.

# Unlikely Changes {#Sec:UCs}

This section lists the unlikely changes to be made to the software.

<div id="ucSRB"></div>

Simulate-Rigid-Bodies: The goal of the system is to simulate the interactions of rigid bodies.

<div id="ucEI"></div>

External-Input: There will always be a source of input data external to the software.

<div id="ucCCS"></div>

Cartesian-Coordinate-System: A Cartesian Coordinate system is used.

<div id="ucORB"></div>

Objects-Rigid-Bodies: All objects are rigid bodies.

# Off-The-Shelf Solutions {#Sec:offShelfSolns}

As mentioned in the [problem description](#Sec:ProbDesc), there already exist free open source game physics libraries. Similar 2D physics libraries are:

- Box2D: http://box2d.org/
- Nape Physics Engine: http://napephys.com/

Free open source 3D game physics libraries include:

- Bullet: http://bulletphysics.org/
- Open Dynamics Engine: http://www.ode.org/
- Newton Game Dynamics: http://newtondynamics.com/

# Traceability Matrices and Graphs {#Sec:TraceMatrices}

The purpose of the traceability matrices is to provide easy references on what has to be additionally modified if a certain component is changed. Every time a component is changed, the items in the column of that component that are marked with an "X" should be modified as well. [Tab:TraceMatAvsA](#Table:TraceMatAvsA) shows the dependencies of the assumptions on each other. [Tab:TraceMatAvsAll](#Table:TraceMatAvsAll) shows the dependencies of the data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Tab:TraceMatRefvsRef](#Table:TraceMatRefvsRef) shows the dependencies of the data definitions, theoretical models, general definitions, and instance models on each other. [Tab:TraceMatAllvsR](#Table:TraceMatAllvsR) shows the dependencies of the requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models.

<div id="Table:TraceMatAvsA"></div>

|                                                |[A:objectTy](#assumpOT)|[A:objectDimension](#assumpOD)|[A:coordinateSystemTy](#assumpCST)|[A:axesDefined](#assumpAD)|[A:collisionType](#assumpCT)|[A:dampingInvolvement](#assumpDI)|[A:constraintsAndJointsInvolvement](#assumpCAJI)|
|:-----------------------------------------------|:----------------------|:-----------------------------|:---------------------------------|:-------------------------|:---------------------------|:--------------------------------|:-----------------------------------------------|
|[A:objectTy](#assumpOT)                         |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:objectDimension](#assumpOD)                  |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:coordinateSystemTy](#assumpCST)              |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:axesDefined](#assumpAD)                      |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:collisionType](#assumpCT)                    |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:dampingInvolvement](#assumpDI)               |                       |                              |                                  |                          |                            |                                 |                                                |
|[A:constraintsAndJointsInvolvement](#assumpCAJI)|                       |                              |                                  |                          |                            |                                 |                                                |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Assumptions</p>**

<div id="Table:TraceMatAvsAll"></div>

|                                                                   |[A:objectTy](#assumpOT)|[A:objectDimension](#assumpOD)|[A:coordinateSystemTy](#assumpCST)|[A:axesDefined](#assumpAD)|[A:collisionType](#assumpCT)|[A:dampingInvolvement](#assumpDI)|[A:constraintsAndJointsInvolvement](#assumpCAJI)|
|:------------------------------------------------------------------|:----------------------|:-----------------------------|:---------------------------------|:-------------------------|:---------------------------|:--------------------------------|:-----------------------------------------------|
|[DD:ctrOfMass](#DD:ctrOfMass)                                      |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:linDisp](#DD:linDisp)                                          |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:linVel](#DD:linVel)                                            |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:linAcc](#DD:linAcc)                                            |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:angDisp](#DD:angDisp)                                          |X                      |X                             |                                  |                          |                            |                                 |                                                |
|[DD:angVel](#DD:angVel)                                            |X                      |X                             |                                  |                          |                            |                                 |                                                |
|[DD:angAccel](#DD:angAccel)                                        |X                      |X                             |                                  |                          |                            |                                 |                                                |
|[DD:chaslesThm](#DD:chaslesThm)                                    |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:torque](#DD:torque)                                            |                       |                              |                                  |                          |                            |                                 |                                                |
|[DD:kEnergy](#DD:kEnergy)                                          |X                      |X                             |                                  |                          |                            |X                                |                                                |
|[DD:coeffRestitution](#DD:coeffRestitution)                        |                       |                              |                                  |                          |                            |                                 |                                                |
|[DD:reVeInColl](#DD:reVeInColl)                                    |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:impulseV](#DD:impulseV)                                        |X                      |                              |                                  |                          |                            |                                 |                                                |
|[DD:potEnergy](#DD:potEnergy)                                      |X                      |X                             |                                  |                          |                            |X                                |                                                |
|[DD:momentOfInertia](#DD:momentOfInertia)                          |X                      |                              |                                  |                          |                            |                                 |                                                |
|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)                          |                       |                              |                                  |                          |                            |                                 |                                                |
|[TM:NewtonThirdLawMot](#TM:NewtonThirdLawMot)                      |                       |                              |                                  |                          |                            |                                 |                                                |
|[TM:UniversalGravLaw](#TM:UniversalGravLaw)                        |                       |                              |                                  |                          |                            |                                 |                                                |
|[TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot)                    |                       |X                             |                                  |                          |                            |                                 |                                                |
|[GD:accelGravity](#GD:accelGravity)                                |                       |                              |                                  |                          |                            |                                 |                                                |
|[GD:impulse](#GD:impulse)                                          |X                      |X                             |                                  |X                         |X                           |                                 |                                                |
|[IM:transMot](#IM:transMot)                                        |X                      |X                             |                                  |                          |                            |X                                |X                                               |
|[IM:rotMot](#IM:rotMot)                                            |X                      |X                             |                                  |X                         |                            |                                 |                                                |
|[IM:col2D](#IM:col2D)                                              |X                      |X                             |                                  |X                         |X                           |X                                |X                                               |
|[FR:Simulation-Space](#simSpace)                                   |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Input-Initial-Conditions](#inputInitialConds)                  |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Input-Surface-Properties](#inputSurfaceProps)                  |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Verify-Physical_Constraints](#verifyPhysCons)                  |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Calculate-Translation-Over-Time](#calcTransOverTime)           |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Calculate-Rotation-Over-Time](#calcRotOverTime)                |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Determine-Collisions](#deterColls)                             |                       |                              |                                  |                          |                            |                                 |                                                |
|[FR:Determine-Collision-Response-Over-Time](#deterCollRespOverTime)|                       |                              |                                  |                          |                            |                                 |                                                |
|[NFR:Performance](#performance)                                    |                       |                              |                                  |                          |                            |                                 |                                                |
|[NFR:Correctness](#correctness)                                    |                       |                              |                                  |                          |                            |                                 |                                                |
|[NFR:Usability](#usability)                                        |                       |                              |                                  |                          |                            |                                 |                                                |
|[NFR:Understandability](#understandability)                        |                       |                              |                                  |                          |                            |                                 |                                                |
|[NFR:Maintainability](#maintainability)                            |                       |                              |                                  |                          |                            |                                 |                                                |
|[LC:Variable-ODE-Solver](#lcVODES)                                 |                       |                              |                                  |                          |                            |                                 |                                                |
|[LC:Expanded-Collisions](#lcEC)                                    |                       |                              |                                  |                          |X                           |                                 |                                                |
|[LC:Include-Dampening](#lcID)                                      |                       |                              |                                  |                          |                            |X                                |                                                |
|[LC:Include-Joints-Constraints](#lcIJC)                            |                       |                              |                                  |                          |                            |                                 |X                                               |
|[UC:Simulate-Rigid-Bodies](#ucSRB)                                 |                       |                              |                                  |                          |                            |                                 |                                                |
|[UC:External-Input](#ucEI)                                         |                       |                              |                                  |                          |                            |                                 |                                                |
|[UC:Cartesian-Coordinate-System](#ucCCS)                           |                       |                              |                                  |                          |                            |                                 |                                                |
|[UC:Objects-Rigid-Bodies](#ucORB)                                  |                       |                              |                                  |                          |                            |                                 |                                                |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Items</p>**

<div id="Table:TraceMatRefvsRef"></div>

|                                               |[DD:ctrOfMass](#DD:ctrOfMass)|[DD:linDisp](#DD:linDisp)|[DD:linVel](#DD:linVel)|[DD:linAcc](#DD:linAcc)|[DD:angDisp](#DD:angDisp)|[DD:angVel](#DD:angVel)|[DD:angAccel](#DD:angAccel)|[DD:chaslesThm](#DD:chaslesThm)|[DD:torque](#DD:torque)|[DD:kEnergy](#DD:kEnergy)|[DD:coeffRestitution](#DD:coeffRestitution)|[DD:reVeInColl](#DD:reVeInColl)|[DD:impulseV](#DD:impulseV)|[DD:potEnergy](#DD:potEnergy)|[DD:momentOfInertia](#DD:momentOfInertia)|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|[TM:NewtonThirdLawMot](#TM:NewtonThirdLawMot)|[TM:UniversalGravLaw](#TM:UniversalGravLaw)|[TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot)|[GD:accelGravity](#GD:accelGravity)|[GD:impulse](#GD:impulse)|[IM:transMot](#IM:transMot)|[IM:rotMot](#IM:rotMot)|[IM:col2D](#IM:col2D)|
|:----------------------------------------------|:----------------------------|:------------------------|:----------------------|:----------------------|:------------------------|:----------------------|:--------------------------|:------------------------------|:----------------------|:------------------------|:------------------------------------------|:------------------------------|:--------------------------|:----------------------------|:----------------------------------------|:----------------------------------------|:--------------------------------------------|:------------------------------------------|:----------------------------------------------|:----------------------------------|:------------------------|:--------------------------|:----------------------|:--------------------|
|[DD:ctrOfMass](#DD:ctrOfMass)                  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:linDisp](#DD:linDisp)                      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:linVel](#DD:linVel)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:linAcc](#DD:linAcc)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:angDisp](#DD:angDisp)                      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:angVel](#DD:angVel)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:angAccel](#DD:angAccel)                    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:chaslesThm](#DD:chaslesThm)                |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:torque](#DD:torque)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:kEnergy](#DD:kEnergy)                      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:coeffRestitution](#DD:coeffRestitution)    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:reVeInColl](#DD:reVeInColl)                |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:impulseV](#DD:impulseV)                    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:potEnergy](#DD:potEnergy)                  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[DD:momentOfInertia](#DD:momentOfInertia)      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[TM:NewtonThirdLawMot](#TM:NewtonThirdLawMot)  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[TM:UniversalGravLaw](#TM:UniversalGravLaw)    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot)|                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[GD:accelGravity](#GD:accelGravity)            |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |X                                          |                                               |                                   |                         |                           |                       |                     |
|[GD:impulse](#GD:impulse)                      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |
|[IM:transMot](#IM:transMot)                    |X                            |X                        |X                      |X                      |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |X                                        |                                             |                                           |                                               |X                                  |                         |                           |                       |                     |
|[IM:rotMot](#IM:rotMot)                        |                             |                         |                       |                       |X                        |X                      |X                          |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |X                                              |                                   |                         |                           |                       |                     |
|[IM:col2D](#IM:col2D)                          |X                            |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |X                        |                           |                       |                     |

**<p align="center">Traceability Matrix Showing the Connections Between Items and Other Sections</p>**

<div id="Table:TraceMatAllvsR"></div>

|                                                                   |[DD:ctrOfMass](#DD:ctrOfMass)|[DD:linDisp](#DD:linDisp)|[DD:linVel](#DD:linVel)|[DD:linAcc](#DD:linAcc)|[DD:angDisp](#DD:angDisp)|[DD:angVel](#DD:angVel)|[DD:angAccel](#DD:angAccel)|[DD:chaslesThm](#DD:chaslesThm)|[DD:torque](#DD:torque)|[DD:kEnergy](#DD:kEnergy)|[DD:coeffRestitution](#DD:coeffRestitution)|[DD:reVeInColl](#DD:reVeInColl)|[DD:impulseV](#DD:impulseV)|[DD:potEnergy](#DD:potEnergy)|[DD:momentOfInertia](#DD:momentOfInertia)|[TM:NewtonSecLawMot](#TM:NewtonSecLawMot)|[TM:NewtonThirdLawMot](#TM:NewtonThirdLawMot)|[TM:UniversalGravLaw](#TM:UniversalGravLaw)|[TM:NewtonSecLawRotMot](#TM:NewtonSecLawRotMot)|[GD:accelGravity](#GD:accelGravity)|[GD:impulse](#GD:impulse)|[IM:transMot](#IM:transMot)|[IM:rotMot](#IM:rotMot)|[IM:col2D](#IM:col2D)|[FR:Simulation-Space](#simSpace)|[FR:Input-Initial-Conditions](#inputInitialConds)|[FR:Input-Surface-Properties](#inputSurfaceProps)|[FR:Verify-Physical_Constraints](#verifyPhysCons)|[FR:Calculate-Translation-Over-Time](#calcTransOverTime)|[FR:Calculate-Rotation-Over-Time](#calcRotOverTime)|[FR:Determine-Collisions](#deterColls)|[FR:Determine-Collision-Response-Over-Time](#deterCollRespOverTime)|[NFR:Performance](#performance)|[NFR:Correctness](#correctness)|[NFR:Usability](#usability)|[NFR:Understandability](#understandability)|[NFR:Maintainability](#maintainability)|
|:------------------------------------------------------------------|:----------------------------|:------------------------|:----------------------|:----------------------|:------------------------|:----------------------|:--------------------------|:------------------------------|:----------------------|:------------------------|:------------------------------------------|:------------------------------|:--------------------------|:----------------------------|:----------------------------------------|:----------------------------------------|:--------------------------------------------|:------------------------------------------|:----------------------------------------------|:----------------------------------|:------------------------|:--------------------------|:----------------------|:--------------------|:-------------------------------|:------------------------------------------------|:------------------------------------------------|:------------------------------------------------|:-------------------------------------------------------|:--------------------------------------------------|:-------------------------------------|:------------------------------------------------------------------|:------------------------------|:------------------------------|:--------------------------|:------------------------------------------|:--------------------------------------|
|[GS:Determine-Linear-Properties](#linearGS)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[GS:Determine-Angular-Properties](#angularGS)                      |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Simulation-Space](#simSpace)                                   |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Input-Initial-Conditions](#inputInitialConds)                  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Input-Surface-Properties](#inputSurfaceProps)                  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Verify-Physical_Constraints](#verifyPhysCons)                  |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Calculate-Translation-Over-Time](#calcTransOverTime)           |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Calculate-Rotation-Over-Time](#calcRotOverTime)                |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Determine-Collisions](#deterColls)                             |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[FR:Determine-Collision-Response-Over-Time](#deterCollRespOverTime)|                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[NFR:Performance](#performance)                                    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[NFR:Correctness](#correctness)                                    |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[NFR:Usability](#usability)                                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[NFR:Understandability](#understandability)                        |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |
|[NFR:Maintainability](#maintainability)                            |                             |                         |                       |                       |                         |                       |                           |                               |                       |                         |                                           |                               |                           |                             |                                         |                                         |                                             |                                           |                                               |                                   |                         |                           |                       |                     |                                |                                                 |                                                 |                                                 |                                                        |                                                   |                                      |                                                                   |                               |                               |                           |                                           |                                       |

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

- [TraceGraphAvsA](../../../../traceygraphs/gamephysics/avsa.svg)
- [TraceGraphAvsAll](../../../../traceygraphs/gamephysics/avsall.svg)
- [TraceGraphRefvsRef](../../../../traceygraphs/gamephysics/refvsref.svg)
- [TraceGraphAllvsR](../../../../traceygraphs/gamephysics/allvsr.svg)
- [TraceGraphAllvsAll](../../../../traceygraphs/gamephysics/allvsall.svg)

# Values of Auxiliary Constants {#Sec:AuxConstants}

There are no auxiliary constants.

# References {#Sec:References}

<div id="jfBeucheIntro"></div>

[1]: Bueche, J. Frederick. *Introduction to Physics for Scientists, Fourth Edition*. Mcgraw-Hill College, 1986.

<div id="koothoor2013"></div>

[2]: Koothoor, Nirmitha. *A Document Driven Approach to Certifying Scientific Computing Software*. McMaster University, Hamilton, ON, Canada: 2013. Print.

<div id="parnas1978"></div>

[3]: Parnas, David L. "Designing Software for Ease of Extension and Contraction." *ICSE '78: Proceedings of the 3rd international conference on Software engineering*. 1978. pp. 264&ndash;277.

<div id="parnasClements1986"></div>

[4]: Parnas, David L. and Clements, P. C. "A rational design process: How and why to fake it." *IEEE Transactions on Software Engineering*, vol. 12, no. 2, Washington, USA: February, 1986. pp. 251&ndash;257. Print.

<div id="pointSource"></div>

[5]: Pierce, Rod. *Point*. May, 2017. <https://www.mathsisfun.com/geometry/point.html>.

<div id="smithKoothoor2016"></div>

[6]: Smith, W. Spencer and Koothoor, Nirmitha. "A Document-Driven Method for Certifying Scientific Computing Software for Use in Nuclear Safety Analysis." * Nuclear Engineering and Technology*, vol. 48, no. 2, April, 2016. <http://www.sciencedirect.com/science/article/pii/S1738573315002582>. pp. 404&ndash;418.

<div id="smithLai2005"></div>

[7]: Smith, W. Spencer and Lai, Lei. "A new requirements template for scientific computing." *Proceedings of the First International Workshop on Situational Requirements Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements Engineering Processes, SREP'05*. Edited by PJ Agerfalk, N. Kraiem, and J. Ralyte, Paris, France: 2005. pp. 107&ndash;121. In conjunction with 13th IEEE International Requirements Engineering Conference,

<div id="smithEtAl2007"></div>

[8]: Smith, W. Spencer, Lai, Lei, and Khedri, Ridha. "Requirements Analysis for Engineering Computation: A Systematic Approach for Improving Software Reliability." *Reliable Computing, Special Issue on Reliable Engineering Computation*, vol. 13, no. 1, February, 2007. <https://doi.org/10.1007/s11155-006-9020-7>. pp. 83&ndash;107.

<div id="lineSource"></div>

[9]: The Editors of Encyclopaedia Britannica. *Line*. June, 2019. <https://www.britannica.com/science/line-mathematics>.

<div id="chaslesWiki"></div>

[10]: Wikipedia Contributors. *Chasles' theorem (kinematics)*. November, 2018. <https://en.wikipedia.org/wiki/Chasles'_theorem_(kinematics)>.

<div id="cartesianWiki"></div>

[11]: Wikipedia Contributors. *Cartesian coordinate system*. June, 2019. <https://en.wikipedia.org/wiki/Cartesian_coordinate_system>.

<div id="dampingSource"></div>

[12]: Wikipedia Contributors. *Damping*. July, 2019. <https://en.wikipedia.org/wiki/Damping_ratio>.

<div id="sciComp2013"></div>

[13]: Wilson, Greg, Aruliah, D. A., Titus, C., Chue Hong, Neil P., Davis, Matt, Guy, Richard T., Haddock, Steven H. D., Huff, Kathryn D., Mitchell, Ian M., Plumblet, Mark D., Waugh, Ben, White, Ethan P., and Wilson, Paul. "Best Practices for Scientific Computing, 2013." *PLoS Biol*, vol. 12, no. 1, 2013. Print.
