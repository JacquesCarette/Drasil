# System Context {#Sec:SysContext}

[Fig:SysCon](./SecSysContext.md#Figure:SysCon) shows the system context. A circle represents an external entity outside the software, the user in this case. A rectangle represents the software system itself (SWHS). Arrows are used to show the data flow between the system and its environment.

<div id="Figure:SysCon"></div>

![[Fig:SysCon](./SecSysContext.md#Figure:SysCon): System Context](./assets/SystemContextFigure.png)

**<p align="center">[Fig:SysCon](./SecSysContext.md#Figure:SysCon): System Context</p>**

SWHS is mostly self-contained. The only external interaction is through the user interface. The responsibilities of the user and the system are as follows:

- User Responsibilities:
  - Provide the input data to the system, ensuring no errors in the data entry
  - Take care that consistent units are used for input variables
- SWHS Responsibilities:
  - Detect data type mismatch, such as a string of characters instead of a floating point number
  - Determine if the inputs satisfy the required physical and software constraints
  - Calculate the required outputs
