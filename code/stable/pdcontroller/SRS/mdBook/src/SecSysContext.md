# System Context {#Sec:SysContext}

[Fig:systemContextDiag](./SecSysContext.md#Figure:systemContextDiag) shows the system context. The circle represents an external entity outside the software, the user in this case. The rectangle represents the software system itself, PD Controller in this case. Arrows are used to show the data flow between the system and its environment.

<div id="Figure:systemContextDiag"></div>

![System Context](../../../../datafiles/pdcontroller/Fig_SystemContext.png)
**<p align="center">System Context</p>**

PD Controller is self-contained. The only external interaction is with the user. The responsibilities of the user and the system are as follows:

- User Responsibilities
  - Feed inputs to the model
  - Review the response of the Power Plant
  - Tune the controller gains
- PD Controller Responsibilities
  - Check the validity of the inputs
  - Calculate the outputs of the PD Controller and Power Plant

