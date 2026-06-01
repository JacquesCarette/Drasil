# System Context

[Fig:sysCtxDiag](./sys-context.md#Figure:sysCtxDiag) shows the system context. A circle represents an entity external to the software, the user in this case. A rectangle represents the software system itself (Projectile). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag">

![System Context Figure](../images/SystemContextFigure.png)
**<p align="center">System Context</p>**

</div>

The interaction between the product and the user is through an application programming interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide initial conditions of the physical state of the motion and the input data related to the Projectile, ensuring no errors in the data entry.
  - Ensure that consistent units are used for input variables.
  - Ensure required [software assumptions](./assumptions.md) are appropriate for any particular problem input to the software.
- Projectile Responsibilities
  - Detect data type mismatch, such as a string of characters input instead of a floating point number.
  - Determine if the inputs satisfy the required physical and software constraints.
  - Calculate the required outputs.

<!-- TODO: add links -->
