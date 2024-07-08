# System Context {#Sec:SysContext}

[Fig:sysCtxDiag](./SecSysContext.md#Figure:sysCtxDiag) shows the system context. A circle represents an entity external to the software, the user in this case. A rectangle represents the software system itself (GamePhysics). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:sysCtxDiag"></div>

![System Context](../../../../../datafiles/gamephysics/sysctx.png)

**<p align="center">System Context</p>**

The interaction between the product and the user is through an application programming interface. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Provide initial conditions of the physical state of the simulation, rigid bodies present, and forces applied to them.
  - Ensure application programming interface use complies with the user guide.
  - Ensure required [software assumptions](./SecAssumps.md#Sec:Assumps) are appropriate for any particular problem the software addresses.
- GamePhysics Responsibilities
  - Determine if the inputs and simulation state satisfy the required [physical and system constraints](./SecDataConstraints.md#Sec:DataConstraints).
  - Calculate the new state of all rigid bodies within the simulation at each simulation step.
  - Provide updated physical state of all rigid bodies at the end of a simulation step.
