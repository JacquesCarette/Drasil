# Traceability Matrices and Graphs {#Sec:TraceMatrices}

The purpose of the traceability matrices is to provide easy references on what has to be additionally modified if a certain component is changed. Every time a component is changed, the items in the column of that component that are marked with an "X" should be modified as well. [Tab:TraceMatAvsA](./SecTraceMatrices.md#Table:TraceMatAvsA) shows the dependencies of the assumptions on each other. [Tab:TraceMatAvsAll](./SecTraceMatrices.md#Table:TraceMatAvsAll) shows the dependencies of the data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Tab:TraceMatRefvsRef](./SecTraceMatrices.md#Table:TraceMatRefvsRef) shows the dependencies of the data definitions, theoretical models, general definitions, and instance models on each other. [Tab:TraceMatAllvsR](./SecTraceMatrices.md#Table:TraceMatAllvsR) shows the dependencies of the requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models.

<div id="Table:TraceMatAvsA"></div>

|||||||||
|:|:|:|:|:|:|:|:|
|||||||||
|||||||||
|||||||||
|||||||||
|||||||||
|||||||||
|||||||||

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Assumptions</p>**

<div id="Table:TraceMatAvsAll"></div>

|                                                          | | || | | | |
|:---------------------------------------------------------|:|:|:|:|:|:|:|
|[TM:NewtonSecLawMot](./SecTMs.md#TM:NewtonSecLawMot)      | | || | | | |
|[TM:NewtonThirdLawMot](./SecTMs.md#TM:NewtonThirdLawMot)  | | || | | | |
|[TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw)    | | || | | | |
|[TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot)| |X|| | | | |
|[GD:accelGravity](./SecGDs.md#GD:accelGravity)            | | || | | | |
|[GD:impulse](./SecGDs.md#GD:impulse)                      |X|X||X|X| | |
|[IM:transMot](./SecIMs.md#IM:transMot)                    |X|X|| | |X|X|
|[IM:rotMot](./SecIMs.md#IM:rotMot)                        |X|X||X| | | |
|[IM:col2D](./SecIMs.md#IM:col2D)                          |X|X||X|X|X|X|
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || |X| | |
|                                                          | | || | |X| |
|                                                          | | || | | |X|
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |
|                                                          | | || | | | |

**<p align="center">Traceability Matrix Showing the Connections Between Assumptions and Other Items</p>**

<div id="Table:TraceMatRefvsRef"></div>

|                                                          |[TM:NewtonSecLawMot](./SecTMs.md#TM:NewtonSecLawMot)|[TM:NewtonThirdLawMot](./SecTMs.md#TM:NewtonThirdLawMot)|[TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw)|[TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot)|[GD:accelGravity](./SecGDs.md#GD:accelGravity)|[GD:impulse](./SecGDs.md#GD:impulse)|[IM:transMot](./SecIMs.md#IM:transMot)|[IM:rotMot](./SecIMs.md#IM:rotMot)|[IM:col2D](./SecIMs.md#IM:col2D)|
|:---------------------------------------------------------|:---------------------------------------------------|:-------------------------------------------------------|:-----------------------------------------------------|:---------------------------------------------------------|:---------------------------------------------|:-----------------------------------|:-------------------------------------|:---------------------------------|:-------------------------------|
|[TM:NewtonSecLawMot](./SecTMs.md#TM:NewtonSecLawMot)      |                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                |
|[TM:NewtonThirdLawMot](./SecTMs.md#TM:NewtonThirdLawMot)  |                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                |
|[TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw)    |                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                |
|[TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot)|                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                |
|[GD:accelGravity](./SecGDs.md#GD:accelGravity)            |                                                    |                                                        |X                                                     |                                                          |                                              |                                    |                                      |                                  |                                |
|[GD:impulse](./SecGDs.md#GD:impulse)                      |                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                |
|[IM:transMot](./SecIMs.md#IM:transMot)                    |X                                                   |                                                        |                                                      |                                                          |X                                             |                                    |                                      |                                  |                                |
|[IM:rotMot](./SecIMs.md#IM:rotMot)                        |                                                    |                                                        |                                                      |X                                                         |                                              |                                    |                                      |                                  |                                |
|[IM:col2D](./SecIMs.md#IM:col2D)                          |                                                    |                                                        |                                                      |                                                          |                                              |X                                   |                                      |                                  |                                |

**<p align="center">Traceability Matrix Showing the Connections Between Items and Other Sections</p>**

<div id="Table:TraceMatAllvsR"></div>

||[TM:NewtonSecLawMot](./SecTMs.md#TM:NewtonSecLawMot)|[TM:NewtonThirdLawMot](./SecTMs.md#TM:NewtonThirdLawMot)|[TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw)|[TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot)|[GD:accelGravity](./SecGDs.md#GD:accelGravity)|[GD:impulse](./SecGDs.md#GD:impulse)|[IM:transMot](./SecIMs.md#IM:transMot)|[IM:rotMot](./SecIMs.md#IM:rotMot)|[IM:col2D](./SecIMs.md#IM:col2D)||||||||||||||
|:|:---------------------------------------------------|:-------------------------------------------------------|:-----------------------------------------------------|:---------------------------------------------------------|:---------------------------------------------|:-----------------------------------|:-------------------------------------|:---------------------------------|:-------------------------------|:|:|:|:|:|:|:|:|:|:|:|:|:|
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||
||                                                    |                                                        |                                                      |                                                          |                                              |                                    |                                      |                                  |                                ||||||||||||||

**<p align="center">Traceability Matrix Showing the Connections Between Requirements, Goal Statements and Other Items</p>**

The purpose of the traceability graphs is also to provide easy references on what has to be additionally modified if a certain component is changed. The arrows in the graphs represent dependencies. The component at the tail of an arrow is depended on by the component at the head of that arrow. Therefore, if a component is changed, the components that it points to should also be changed. [Fig:TraceGraphAvsA](./SecTraceMatrices.md#Figure:TraceGraphAvsA) shows the dependencies of assumptions on each other. [Fig:TraceGraphAvsAll](./SecTraceMatrices.md#Figure:TraceGraphAvsAll) shows the dependencies of data definitions, theoretical models, general definitions, instance models, requirements, likely changes, and unlikely changes on the assumptions. [Fig:TraceGraphRefvsRef](./SecTraceMatrices.md#Figure:TraceGraphRefvsRef) shows the dependencies of data definitions, theoretical models, general definitions, and instance models on each other. [Fig:TraceGraphAllvsR](./SecTraceMatrices.md#Figure:TraceGraphAllvsR) shows the dependencies of requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models. [Fig:TraceGraphAllvsAll](./SecTraceMatrices.md#Figure:TraceGraphAllvsAll) shows the dependencies of dependencies of assumptions, models, definitions, requirements, goals, and changes with each other.

<div id="Figure:TraceGraphAvsA" align="center" >

![TraceGraphAvsA](./assets/avsa.svg)

**Figure: TraceGraphAvsA**

</div>

<div id="Figure:TraceGraphAvsAll" align="center" >

![TraceGraphAvsAll](./assets/avsall.svg)

**Figure: TraceGraphAvsAll**

</div>

<div id="Figure:TraceGraphRefvsRef" align="center" >

![TraceGraphRefvsRef](./assets/refvsref.svg)

**Figure: TraceGraphRefvsRef**

</div>

<div id="Figure:TraceGraphAllvsR" align="center" >

![TraceGraphAllvsR](./assets/allvsr.svg)

**Figure: TraceGraphAllvsR**

</div>

<div id="Figure:TraceGraphAllvsAll" align="center" >

![TraceGraphAllvsAll](./assets/allvsall.svg)

**Figure: TraceGraphAllvsAll**

</div>

For convenience, the following graphs can be found at the links below:

- [TraceGraphAvsA](../../../../traceygraphs/gamephysics/avsa.svg)
- [TraceGraphAvsAll](../../../../traceygraphs/gamephysics/avsall.svg)
- [TraceGraphRefvsRef](../../../../traceygraphs/gamephysics/refvsref.svg)
- [TraceGraphAllvsR](../../../../traceygraphs/gamephysics/allvsr.svg)
- [TraceGraphAllvsAll](../../../../traceygraphs/gamephysics/allvsall.svg)
