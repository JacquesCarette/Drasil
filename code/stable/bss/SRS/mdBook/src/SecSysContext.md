# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](./SecSysContext.md#Figure:sysCtxDiag) shows the system context. A circle represents an entity external to the software, the user in this case. A rectangle represents the software system itself (BSS). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag" align="center" >

![System Context](./assets/SystemContextFigure.png)

**Figure: System Context**

</div>

The interaction between the product and the user is through an application programming interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide the physical parameters of the binary star system, including stellar masses, initial positions, and initial velocities, ensuring no errors in the data entry.
  - Ensure that consistent units are used for inputs.
  - Ensure required [software assumptions](./SecAssumps.md#Sec:Assumps) are appropriate for any particular problem input to the software.
- BSS Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number.
  - Determine if the inputs satisfy the required physical and software constraints.
  - Calculate the required outputs.
