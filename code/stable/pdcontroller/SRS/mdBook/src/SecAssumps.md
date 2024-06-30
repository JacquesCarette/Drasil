# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="pwrPlant"></div>

Power plant: The Power Plant and the Sensor are coupled as a single unit. (RefBy: [A:Spring Stiffness Coefficient](./SecAssumps.md#stiffnessCoeffSpring), [A:Transfer Function](./SecAssumps.md#pwrPlantTxFnx), [A:Spring Mass](./SecAssumps.md#massSpring), and [A:Spring Damping Coefficient](./SecAssumps.md#dampingCoeffSpring).)

<div id="decoupled"></div>

Decoupled equation: The decoupled form of the PD Controller equation used in this simulation. (RefBy: [DD:ddCtrlVar](./SecDDs.md#DD:ddCtrlVar).)

<div id="setPoint"></div>

Set-Point: The Set-Point is constant throughout the simulation. (RefBy: [IM:pdEquationIM](./SecIMs.md#IM:pdEquationIM) and [DD:ddProcessError](./SecDDs.md#DD:ddProcessError).)

<div id="externalDisturb"></div>

External disturbance: There are no external disturbances to the Power Plant during the simulation. (RefBy: [GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant).)

<div id="initialValue"></div>

Initial Value: The initial value of the Process Variable is assumed to be zero. (RefBy: [DD:ddProcessError](./SecDDs.md#DD:ddProcessError).)

<div id="parallelEq"></div>

Parallel Equation: The Parallel form of the equation is used for the PD Controller. (RefBy: [DD:ddCtrlVar](./SecDDs.md#DD:ddCtrlVar).)

<div id="unfilteredDerivative"></div>

Unfiltered Derivative: A pure derivative function is used for this simulation; there are no filters applied. (RefBy: [DD:ddDerivCtrl](./SecDDs.md#DD:ddDerivCtrl).)

<div id="pwrPlantTxFnx"></div>

Transfer Function: The combined Power Plant and Sensor ([A:Power plant](./SecAssumps.md#pwrPlant)) are characterized by a Second Order mass-spring-damper System. (RefBy: [TM:tmSOSystem](./SecTMs.md#TM:tmSOSystem).)

<div id="massSpring"></div>

Spring Mass: The mass of the spring in the mass-spring-damper system ([A:Power plant](./SecAssumps.md#pwrPlant)) is assumed to be 1 kilogram. (RefBy: [GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant) and [LC:DC Gain and Time Constant](./SecLCs.md#likeChgPP).)

<div id="dampingCoeffSpring"></div>

Spring Damping Coefficient: The Damping Coefficient of the spring in the mass-spring-damper system ([A:Power plant](./SecAssumps.md#pwrPlant)) is assumed to be 1. (RefBy: [GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant) and [LC:DC Gain and Time Constant](./SecLCs.md#likeChgPP).)

<div id="stiffnessCoeffSpring"></div>

Spring Stiffness Coefficient: The Stiffness Coefficient of the spring in the mass-spring-damper system ([A:Power plant](./SecAssumps.md#pwrPlant)) is assumed to be 20. (RefBy: [GD:gdPowerPlant](./SecGDs.md#GD:gdPowerPlant) and [LC:DC Gain and Time Constant](./SecLCs.md#likeChgPP).)


