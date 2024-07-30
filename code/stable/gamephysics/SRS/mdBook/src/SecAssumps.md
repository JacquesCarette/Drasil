# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="assumpOT"></div>

objectTy: All objects are rigid bodies. (RefBy: [GD:impulse](./SecGDs.md#GD:impulse), [IM:rotMot](./SecIMs.md#IM:rotMot), [IM:transMot](./SecIMs.md#IM:transMot), [DD:chaslesThm](./SecDDs.md#DD:chaslesThm), [DD:reVeInColl](./SecDDs.md#DD:reVeInColl), [DD:potEnergy](./SecDDs.md#DD:potEnergy), [DD:ctrOfMass](./SecDDs.md#DD:ctrOfMass), [DD:momentOfInertia](./SecDDs.md#DD:momentOfInertia), [DD:linVel](./SecDDs.md#DD:linVel), [DD:linDisp](./SecDDs.md#DD:linDisp), [DD:linAcc](./SecDDs.md#DD:linAcc), [DD:kEnergy](./SecDDs.md#DD:kEnergy), [DD:impulseV](./SecDDs.md#DD:impulseV), [IM:col2D](./SecIMs.md#IM:col2D), [DD:angVel](./SecDDs.md#DD:angVel), [DD:angDisp](./SecDDs.md#DD:angDisp), and [DD:angAccel](./SecDDs.md#DD:angAccel).)

<div id="assumpOD"></div>

objectDimension: All objects are 2D. (RefBy: [GD:impulse](./SecGDs.md#GD:impulse), [IM:rotMot](./SecIMs.md#IM:rotMot), [IM:transMot](./SecIMs.md#IM:transMot), [DD:potEnergy](./SecDDs.md#DD:potEnergy), [TM:NewtonSecLawRotMot](./SecTMs.md#TM:NewtonSecLawRotMot), [DD:kEnergy](./SecDDs.md#DD:kEnergy), [IM:col2D](./SecIMs.md#IM:col2D), [DD:angVel](./SecDDs.md#DD:angVel), [DD:angDisp](./SecDDs.md#DD:angDisp), and [DD:angAccel](./SecDDs.md#DD:angAccel).)

<div id="assumpCST"></div>

coordinateSystemTy: The library uses a Cartesian coordinate system.

<div id="assumpAD"></div>

axesDefined: The axes are defined using right-handed coordinate system. (RefBy: [GD:impulse](./SecGDs.md#GD:impulse), [IM:rotMot](./SecIMs.md#IM:rotMot), and [IM:col2D](./SecIMs.md#IM:col2D).)

<div id="assumpCT"></div>

collisionType: All rigid bodies collisions are vertex-to-edge collisions. (RefBy: [GD:impulse](./SecGDs.md#GD:impulse), [LC:Expanded-Collisions](./SecLCs.md#lcEC), and [IM:col2D](./SecIMs.md#IM:col2D).)

<div id="assumpDI"></div>

dampingInvolvement: There is no damping involved throughout the simulation and this implies that there are no friction forces. (RefBy: [IM:transMot](./SecIMs.md#IM:transMot), [DD:potEnergy](./SecDDs.md#DD:potEnergy), [LC:Include-Dampening](./SecLCs.md#lcID), [DD:kEnergy](./SecDDs.md#DD:kEnergy), and [IM:col2D](./SecIMs.md#IM:col2D).)

<div id="assumpCAJI"></div>

constraintsAndJointsInvolvement: There are no constraints and joints involved throughout the simulation. (RefBy: [IM:transMot](./SecIMs.md#IM:transMot), [LC:Include-Joints-Constraints](./SecLCs.md#lcIJC), and [IM:col2D](./SecIMs.md#IM:col2D).)
