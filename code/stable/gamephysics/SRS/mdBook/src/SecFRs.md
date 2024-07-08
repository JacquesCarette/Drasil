# Functional Requirements {#Sec:FRs}

This section provides the functional requirements, the tasks and behaviours that the software is expected to complete.

<div id="simSpace"></div>

Simulation-Space: Create a space for all of the rigid bodies in the physical simulation to interact in.

<div id="inputInitialConds"></div>

Input-Initial-Conditions: Input the initial masses, velocities, orientations, angular velocities of, and forces applied on rigid bodies.

<div id="inputSurfaceProps"></div>

Input-Surface-Properties: Input the surface properties of the bodies such as friction or elasticity.

<div id="verifyPhysCons"></div>

Verify-Physical_Constraints: Verify that the inputs satisfy the required physical constraints from the [solution characteristics specification](./SecSolCharSpec.md#Sec:SolCharSpec).

<div id="calcTransOverTime"></div>

Calculate-Translation-Over-Time: Determine the positions and velocities over a period of time of the 2D rigid bodies acted upon by a force.

<div id="calcRotOverTime"></div>

Calculate-Rotation-Over-Time: Determine the orientations and angular velocities over a period of time of the 2D rigid bodies.

<div id="deterColls"></div>

Determine-Collisions: Determine if any of the rigid bodies in the space have collided.

<div id="deterCollRespOverTime"></div>

Determine-Collision-Response-Over-Time: Determine the positions and velocities over a period of time of the 2D rigid bodies that have undergone a collision.
