# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="assumpSSC"></div>

Slip-Surface-Concave: The slip surface is concave with respect to the slope surface. The (\\({\boldsymbol{x}\_{\text{slip}}}\\), \\({\boldsymbol{y}\_{\text{slip}}}\\)) coordinates of a slip surface follow a concave up function. (RefBy: [IM:crtSlpId](./SecIMs.md#IM:crtSlpId).)

<div id="assumpFOS"></div>

Factor-of-Safety: The factor of safety is assumed to be constant across the entire slip surface. (RefBy: [GD:mobShr](./SecGDs.md#GD:mobShr).)

<div id="assumpSLH"></div>

Soil-Layer-Homogeneous: The soil mass is homogeneous, with consistent soil properties throughout. (RefBy: [GD:sliceWght](./SecGDs.md#GD:sliceWght), [GD:resShr](./SecGDs.md#GD:resShr), and [LC:Calculate-Inhomogeneous-Soil-Layers](./SecLCs.md#LC_inhomogeneous).)

<div id="assumpSP"></div>

Soil-Properties: The soil properties are independent of dry or saturated conditions, with the exception of unit weight. (RefBy: [GD:resShr](./SecGDs.md#GD:resShr).)

<div id="assumpSLI"></div>

Soil-Layers-Isotropic: The soil mass is treated as if the effective cohesion and effective angle of friction are isotropic properties. (RefBy: [GD:resShr](./SecGDs.md#GD:resShr).)

<div id="assumpINSFL"></div>

Interslice-Norm-Shear-Forces-Linear: Following the assumption of Morgenstern and Price ([morgenstern1965](./SecReferences.md#morgenstern1965)), interslice normal forces and interslice shear forces have a proportional relationship, depending on a proportionality constant (\\(λ\\)) and a function (\\(\boldsymbol{f}\\)) describing variation depending on \\(x\\) position. (RefBy: [GD:normShrR](./SecGDs.md#GD:normShrR), [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [UC:Normal-And-Shear-Linear-Only](./SecUCs.md#UC_normshearlinear).)

<div id="assumpPSC"></div>

Plane-Strain-Conditions: The slope and slip surface extends far into and out of the geometry (\\(z\\) coordinate). This implies plane strain conditions, making 2D analysis appropriate. (RefBy: [GD:srfWtrF](./SecGDs.md#GD:srfWtrF), [GD:sliceWght](./SecGDs.md#GD:sliceWght), [GD:resShr](./SecGDs.md#GD:resShr), [GD:effNormF](./SecGDs.md#GD:effNormF), and [GD:baseWtrF](./SecGDs.md#GD:baseWtrF).)

<div id="assumpENSL"></div>

Effective-Norm-Stress-Large: The effective normal stress is large enough that the shear strength to effective normal stress relationship can be approximated as a linear relationship. (RefBy: [TM:equilibrium](./SecTMs.md#TM:equilibrium) and [UC:2D-Analysis-Only](./SecUCs.md#UC_2donly).)

<div id="assumpSBSBISL"></div>

Surface-Base-Slice-between-Interslice-Straight-Lines: The surface and base of a slice are approximated as straight lines. (RefBy: [GD:srfWtrF](./SecGDs.md#GD:srfWtrF), [GD:sliceWght](./SecGDs.md#GD:sliceWght), [GD:baseWtrF](./SecGDs.md#GD:baseWtrF), [TM:mcShrStrgth](./SecTMs.md#TM:mcShrStrgth), [DD:slcHeight](./SecDDs.md#DD:slcHeight), [DD:angleB](./SecDDs.md#DD:angleB), and [DD:angleA](./SecDDs.md#DD:angleA).)

<div id="assumpES"></div>

Edge-Slices: The interslice forces at the 0th and \\(n\\)th interslice interfaces are zero. (RefBy: [IM:intsliceFs](./SecIMs.md#IM:intsliceFs), [IM:fctSfty](./SecIMs.md#IM:fctSfty), and [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor).)

<div id="assumpSF"></div>

Seismic-Force: There is no seismic force acting on the slope. (RefBy: [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [LC:Calculate-Seismic-Force](./SecLCs.md#LC_seismic).)

<div id="assumpSL"></div>

Surface-Load: There is no imposed surface load, and therefore no external forces, acting on the slope. (RefBy: [IM:fctSfty](./SecIMs.md#IM:fctSfty), [IM:nrmShrFor](./SecIMs.md#IM:nrmShrFor), and [LC:Calculate-External-Force](./SecLCs.md#LC_external).)

<div id="assumpWIBE"></div>

Water-Intersects-Base-Edge: The water table only intersects the base of a slice at an edge of the slice. (RefBy: [GD:sliceWght](./SecGDs.md#GD:sliceWght) and [GD:baseWtrF](./SecGDs.md#GD:baseWtrF).)

<div id="assumpWISE"></div>

Water-Intersects-Surface-Edge: The water table only intersects the slope surface at the edge of a slice. (RefBy: [GD:srfWtrF](./SecGDs.md#GD:srfWtrF) and [GD:sliceWght](./SecGDs.md#GD:sliceWght).)

<div id="assumpNESSS"></div>

Negligible-Effect-Surface-Slope-Seismic: The effect of the slope of the surface of the soil on the seismic force is assumed to be negligible. (RefBy: [GD:momentEql](./SecGDs.md#GD:momentEql).)

<div id="assumpHFSM"></div>

Hydrostatic-Force-Slice-Midpoint: The resultant surface hydrostatic forces act into the midpoint of each slice surface and the resultant base hydrostatic forces act into the midpoint of each slice base. (RefBy: [GD:srfWtrF](./SecGDs.md#GD:srfWtrF), [GD:momentEql](./SecGDs.md#GD:momentEql), and [GD:baseWtrF](./SecGDs.md#GD:baseWtrF).)
