# Likely Changes {#Sec:LCs}

This section lists the Likely Changes (LC) to be made to the software.

<div id="likeChgUTP"></div>

Uniform-Temperature-PCM: [A:Temp-PCM-Constant-Across-Volume](./SecAssumps.md#assumpTPCAV) - PCM is actually a poor thermal conductor, so the assumption of uniform temperature of the phase change material is not likely.

<div id="likeChgTCVOD"></div>

Temperature-Coil-Variable-Over-Day: [A:Temp-Heating-Coil-Constant-over-Time](./SecAssumps.md#assumpTHCCoT) - The temperature of the heating coil will change over the course of the day, depending on the energy received from the sun.

<div id="likeChgTCVOL"></div>

Temperature-Coil-Variable-Over-Length: [A:Temp-Heating-Coil-Constant-over-Length](./SecAssumps.md#assumpTHCCoL) - The temperature of the heating coil will actually change along its length as the water within it cools.

<div id="likeChgDT"></div>

Discharging-Tank: [A:Charging-Tank-No-Temp-Discharge](./SecAssumps.md#assumpCTNOD) - The model currently only accounts for charging of the tank. A more complete model would also account for discharging of the tank.

<div id="likeChgDITPW"></div>

Different-Initial-Temps-PCM-Water: [A:Same-Initial-Temp-Water-PCM](./SecAssumps.md#assumpSITWP) - To add more flexibility to the simulation, the initial temperature of the water and the PCM could be allowed to have different values.

<div id="likeChgTLH"></div>

Tank-Lose-Heat: [A:Perfect-Insulation-Tank](./SecAssumps.md#assumpPIT) - Any real tank cannot be perfectly insulated and will lose heat.
