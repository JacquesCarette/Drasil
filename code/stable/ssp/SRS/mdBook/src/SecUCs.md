# Unlikely Changes {#Sec:UCs}

This section lists the unlikely changes to be made to the software.

<div id="UC_normshearlinear"></div>

Normal-And-Shear-Linear-Only: Changes related to [A:Interslice-Norm-Shear-Forces-Linear](./SecAssumps.md#assumpINSFL) are not possible due to the dependency of the calculations on the linear relationship between interslice normal forces and interslice shear forces.

<div id="UC_2donly"></div>

2D-Analysis-Only: [A:Effective-Norm-Stress-Large](./SecAssumps.md#assumpENSL) allows for 2D analysis with these models only because stress along the \\(z\\)-direction is zero. These models do not take into account stress in the \\(z\\)-direction, and therefore cannot be used without manipulation to attempt three-dimensional analysis.


