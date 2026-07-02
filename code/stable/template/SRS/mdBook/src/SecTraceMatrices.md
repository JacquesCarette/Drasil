# Traceability Matrices and Graphs {#Sec:TraceMatrices}

The purpose of the traceability matrices is to provide easy references on what has to be additionally modified if a certain component is changed. Every time a component is changed, the items in the column of that component that are marked with an "X" should be modified as well. [Tab:TraceMatRefvsRef](./SecTraceMatrices.md#Table:TraceMatRefvsRef) shows the dependencies of the data definitions, theoretical models, general definitions, and instance models on each other. [Tab:TraceMatAllvsR](./SecTraceMatrices.md#Table:TraceMatAllvsR) shows the dependencies of the requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models.

<div id="Table:TraceMatRefvsRef"></div>

|                          |[DD:t1](./SecDDs.md#DD:t1)|
|:-------------------------|:-------------------------|
|[DD:t1](./SecDDs.md#DD:t1)|                          |

**<p align="center">Traceability Matrix Showing the Connections Between Items and Other Sections</p>**

<div id="Table:TraceMatAllvsR"></div>

|                                            |[DD:t1](./SecDDs.md#DD:t1)|[FR:Input-Values](./SecFRs.md#inputValues)|[FR:Output-Values](./SecFRs.md#outputValues)|
|:-------------------------------------------|:-------------------------|:-----------------------------------------|:-------------------------------------------|
|[FR:Input-Values](./SecFRs.md#inputValues)  |                          |                                          |                                            |
|[FR:Output-Values](./SecFRs.md#outputValues)|                          |                                          |                                            |

**<p align="center">Traceability Matrix Showing the Connections Between Requirements and Other Items</p>**

The purpose of the traceability graphs is also to provide easy references on what has to be additionally modified if a certain component is changed. The arrows in the graphs represent dependencies. The component at the tail of an arrow is depended on by the component at the head of that arrow. Therefore, if a component is changed, the components that it points to should also be changed. [Fig:TraceGraphAvsA](./SecTraceMatrices.md#Figure:TraceGraphAvsA) shows the dependencies of data definitions, theoretical models, general definitions, and instance models on each other. [Fig:TraceGraphAvsAll](./SecTraceMatrices.md#Figure:TraceGraphAvsAll) shows the dependencies of requirements and goal statements on the data definitions, theoretical models, general definitions, and instance models. [Fig:TraceGraphRefvsRef](./SecTraceMatrices.md#Figure:TraceGraphRefvsRef) shows the dependencies of dependencies of assumptions, models, definitions, requirements, goals, and changes with each other.

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

- [TraceGraphAvsA](../../TraceyGraph/avsa.svg)
- [TraceGraphAvsAll](../../TraceyGraph/avsall.svg)
- [TraceGraphRefvsRef](../../TraceyGraph/refvsref.svg)
- [TraceGraphAllvsR](../../TraceyGraph/allvsr.svg)
- [TraceGraphAllvsAll](../../TraceyGraph/allvsall.svg)
