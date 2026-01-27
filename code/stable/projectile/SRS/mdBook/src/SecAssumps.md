# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="twoDMotion"></div>

twoDMotion: The projectile motion is two-dimensional (2D). (RefBy: [GD:velVec](./SecGDs.md#GD:velVec) and [GD:posVec](./SecGDs.md#GD:posVec).)

<div id="cartSyst"></div>

cartSyst: A Cartesian coordinate system is used (from [A:neglectCurv](./SecAssumps.md#neglectCurv)). (RefBy: [GD:velVec](./SecGDs.md#GD:velVec) and [GD:posVec](./SecGDs.md#GD:posVec).)

<div id="yAxisGravity"></div>

yAxisGravity: The direction of the \\(y\\)-axis is directed opposite to gravity. (RefBy: [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist), [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime), and [A:accelYGravity](./SecAssumps.md#accelYGravity).)

<div id="launchOrigin"></div>

launchOrigin: The launcher is coincident with the origin. (RefBy: [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist) and [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime).)

<div id="targetXAxis"></div>

targetXAxis: The target lies on the \\(x\\)-axis (from [A:neglectCurv](./SecAssumps.md#neglectCurv)). (RefBy: [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime).)

<div id="posXDirection"></div>

posXDirection: The positive \\(x\\)-direction is from the launcher to the target. (RefBy: [IM:offsetIM](./SecIMs.md#IM:offsetIM), [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist), and [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime).)

<div id="constAccel"></div>

constAccel: The acceleration is constant (from [A:accelXZero](./SecAssumps.md#accelXZero), [A:accelYGravity](./SecAssumps.md#accelYGravity), [A:neglectDrag](./SecAssumps.md#neglectDrag), and [A:freeFlight](./SecAssumps.md#freeFlight)). (RefBy: [GD:velVec](./SecGDs.md#GD:velVec) and [GD:posVec](./SecGDs.md#GD:posVec).)

<div id="accelXZero"></div>

accelXZero: The acceleration in the \\(x\\)-direction is zero. (RefBy: [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist) and [A:constAccel](./SecAssumps.md#constAccel).)

<div id="accelYGravity"></div>

accelYGravity: The acceleration in the \\(y\\)-direction is the acceleration due to gravity (from [A:yAxisGravity](./SecAssumps.md#yAxisGravity)). (RefBy: [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime) and [A:constAccel](./SecAssumps.md#constAccel).)

<div id="neglectDrag"></div>

neglectDrag: Air drag is neglected. (RefBy: [A:constAccel](./SecAssumps.md#constAccel) and [LC:Consider-Air-Drag](./SecLCs.md#considerAirDrag).)

<div id="pointMass"></div>

pointMass: The size and shape of the projectile are negligible, so that it can be modelled as a point mass. (RefBy: [GD:rectVel](./SecGDs.md#GD:rectVel) and [GD:rectPos](./SecGDs.md#GD:rectPos).)

<div id="freeFlight"></div>

freeFlight: The flight is free; there are no collisions during the trajectory of the projectile. (RefBy: [A:constAccel](./SecAssumps.md#constAccel).)

<div id="neglectCurv"></div>

neglectCurv: The distance is small enough that the curvature of the celestial body can be neglected. (RefBy: [A:targetXAxis](./SecAssumps.md#targetXAxis) and [A:cartSyst](./SecAssumps.md#cartSyst).)

<div id="timeStartZero"></div>

timeStartZero: Time starts at zero. (RefBy: [GD:velVec](./SecGDs.md#GD:velVec), [GD:rectVel](./SecGDs.md#GD:rectVel), [GD:rectPos](./SecGDs.md#GD:rectPos), [GD:posVec](./SecGDs.md#GD:posVec), and [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime).)

<div id="gravAccelValue"></div>

gravAccelValue: The acceleration due to gravity is assumed to have the value provided in the section for [Values of Auxiliary Constants](./SecAuxConstants.md#Sec:AuxConstants). (RefBy: [IM:calOfLandingDist](./SecIMs.md#IM:calOfLandingDist) and [IM:calOfLandingTime](./SecIMs.md#IM:calOfLandingTime).)
