# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](./SecSysContext.md#Figure:sysCtxDiag) shows the system context. A circle represents an external entity outside the software, the user in this case. A rectangle represents the software system itself (GlassBR). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag"></div>

![System Context](../../../../../datafiles/glassbr/SystemContextFigure.png)

**<p align="center">System Context</p>**

The interaction between the product and the user is through a user interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide the input data related to the glass slab and blast type, ensuring no errors in the data entry.
  - Ensure that consistent units are used for input variables.
  - Ensure required [software assumptions](./SecAssumps.md#Sec:Assumps) are appropriate for any particular problem input to the software.
- GlassBR Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number.
  - Determine if the inputs satisfy the required physical and software constraints.
  - Predict whether the glass slab is safe or not.
