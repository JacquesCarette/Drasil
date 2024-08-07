# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](./SecSysContext.md#Figure:sysCtxDiag) shows the system context. A circle represents an external entity outside the software. A rectangle represents the software system itself (SSP). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag" align="center" >

![Figure: System Context](./assets/SystemContextFigure.png)

**Figure: System Context**

</div>

The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide the input data related to the soil layer(s) and water table (if applicable), ensuring conformation to input data format required by SSP
  - Ensure that consistent units are used for input variables
  - Ensure required [software assumptions](./SecAssumps.md#Sec:Assumps) are appropriate for the problem to which the user is applying the software
- SSP Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number
  - Verify that the inputs satisfy the required physical and other [data constraints](./SecDataConstraints.md#Sec:DataConstraints)
  - Identify the critical slip surface within the possible input range
  - Find the factor of safety for the slope
  - Find the interslice normal force and shear force along the critical slip surface
