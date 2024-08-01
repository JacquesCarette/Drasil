# Assumptions {#Sec:Assumps}

This section simplifies the original problem and helps in developing the theoretical models by filling in the missing information for the physical system. The assumptions refine the scope by providing more detail.

<div id="assumpGT"></div>

glassType: The standard E1300-09a for calculation applies only to monolithic, laminated, or insulating glass constructions of rectangular shape with continuous lateral support along one, two, three, or four edges. This practice assumes that: (1) the supported glass edges for two, three and four-sided support conditions are simply supported and free to slip in plane; (2) glass supported on two sides acts as a simply supported beam; and (3) glass supported on one side acts as a cantilever.

<div id="assumpGC"></div>

glassCondition: Following [astm2009](./SecReferences.md#astm2009) (pg. 1), this practice does not apply to any form of wired, patterned, etched, sandblasted, drilled, notched, or grooved glass with surface and edge treatments that alter the glass strength. (RefBy: [UC:Accommodate-Altered-Glass](./SecUCs.md#accAlteredGlass).)

<div id="assumpES"></div>

explainScenario: This system only considers the external explosion scenario for its calculations. (RefBy: [LC:Calculate-Internal-Blast-Risk](./SecLCs.md#calcInternalBlastRisk).)

<div id="assumpSV"></div>

standardValues: The values provided in [Sec:Values of Auxiliary Constants](./SecAuxConstants.md#Sec:AuxConstants) are assumed for the duration of load (\\({t\_{\text{d}}}\\)), and the material properties of \\(m\\), \\(k\\), and \\(E\\). (RefBy: [IM:sdfTol](./SecIMs.md#IM:sdfTol), [IM:nFL](./SecIMs.md#IM:nFL), [IM:dimlessLoad](./SecIMs.md#IM:dimlessLoad), [LC:Variable-Values-of-m,k,E](./SecLCs.md#varValsOfmkE), [DD:loadDurFactor](./SecDDs.md#DD:loadDurFactor), and [A:ldfConstant](./SecAssumps.md#assumpLDFC).)

<div id="assumpGL"></div>

glassLite: Glass under consideration is assumed to be a single lite; hence, the value of LSF is equal to 1 for all calculations in GlassBR. (RefBy: [LC:Accomodate-More-than-Single-Lite](./SecLCs.md#accMoreThanSingleLite).)

<div id="assumpBC"></div>

boundaryConditions: Boundary conditions for the glass slab are assumed to be 4-sided support for calculations. (RefBy: [LC:Accomodate-More-Boundary-Conditions](./SecLCs.md#accMoreBoundaryConditions).)

<div id="assumpRT"></div>

responseType: The response type considered in GlassBR is flexural. (RefBy: [LC:Consider-More-than-Flexure-Glass](./SecLCs.md#considerMoreThanFlexGlass).)

<div id="assumpLDFC"></div>

ldfConstant: With reference to [A:standardValues](./SecAssumps.md#assumpSV), the value of load duration factor (\\(\mathit{LDF}\\)) is a constant in GlassBR. (RefBy: [LC:Variable-Values-of-m,k,E](./SecLCs.md#varValsOfmkE) and [DD:loadDurFactor](./SecDDs.md#DD:loadDurFactor).)
