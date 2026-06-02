# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="twoBody"></div>

twoBody: The system consists of exactly two stars with masses \\({m\_{1}}\\) and \\({m\_{2}}\\) . Third-body gravitational perturbations are ignored. (RefBy: [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="isolated"></div>

isolated: Non-gravitational forces (e.g., drag, thrust, radiation pressure) are neglected; the only interaction modeled is mutual Newtonian gravitation between the two stars (from [A:newtonianGravity](./SecAssumps.md#newtonianGravity)). (RefBy: [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="newtonianGravity"></div>

newtonianGravity: The gravitational interaction between the stars is modeled using Newton's law of universal gravitation, with the gravitational constant \\(G\\) provided in [Values of Auxiliary Constants](./SecAuxConstants.md#Sec:AuxConstants). (RefBy: [TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw), [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), [UC:Newtonian-Gravity-Model](./SecUCs.md#ucNewtonianGravity), [A:nonzeroSeparation](./SecAssumps.md#nonzeroSeparation), [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs), and [A:isolated](./SecAssumps.md#isolated).)

<div id="nonRelativistic"></div>

nonRelativistic: The motion is modeled using classical (non-relativistic) mechanics; relativistic effects are neglected. (RefBy: [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="pointMass"></div>

pointMass: Each star (\\({m\_{1}}\\), \\({m\_{2}}\\)) is modeled as a point mass, and effects due to stellar size, deformation, or rotation are neglected. (RefBy: [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), [A:nonzeroSeparation](./SecAssumps.md#nonzeroSeparation), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="constantMass"></div>

constantMass: The masses \\({m\_{1}}\\) and \\({m\_{2}}\\) remain constant over time. (RefBy: [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="inertialFrame"></div>

inertialFrame: The simulation is performed in an inertial reference frame. (RefBy: [TM:centerOfMass](./SecTMs.md#TM:centerOfMass), [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="planar"></div>

planar: The motion of the binary star system is confined to a two-dimensional plane. (RefBy: [TM:relPosAndSep](./SecTMs.md#TM:relPosAndSep), [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)

<div id="nonzeroSeparation"></div>

nonzeroSeparation: Collisions are out of scope: the separation distance \\({r\_{12}}\\) is positive for all simulated times, so the gravitational force model remains well-defined (from [A:newtonianGravity](./SecAssumps.md#newtonianGravity) and [A:pointMass](./SecAssumps.md#pointMass)). (RefBy: [TM:UniversalGravLaw](./SecTMs.md#TM:UniversalGravLaw), [IM:accelY2](./SecIMs.md#IM:accelY2), [IM:accelY1](./SecIMs.md#IM:accelY1), [IM:accelX2](./SecIMs.md#IM:accelX2), [IM:accelX1](./SecIMs.md#IM:accelX1), and [LC:Derived-Output-Quantities](./SecLCs.md#lcDerivedOutputs).)
