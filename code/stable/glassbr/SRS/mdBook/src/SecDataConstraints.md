# Data Constraints {#Sec:DataConstraints}

The [Input Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario. The [auxiliary constants](./SecAuxConstants.md#Sec:AuxConstants) give the values of the specification parameters used in the [Input Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints).

<div id="Table:InDataConstraints"></div>

|Var                            |Physical Constraints                         |Software Constraints                                                               |Typical Value             |Uncert.     |
|:------------------------------|:--------------------------------------------|:----------------------------------------------------------------------------------|:-------------------------|:-----------|
|\\(a\\)                        |\\(a\gt{}0\land{}a\geq{}b\\)                 |\\({d\_{\text{min}}}\leq{}a\leq{}{d\_{\text{max}}}\\)                              |\\(1.5\\) \\({\text{m}}\\)|10\\(\\%\\) |
|\\(\mathit{AR}\\)              |\\(\mathit{AR}\geq{}1\\)                     |\\(\mathit{AR}\leq{}{\mathit{AR}\_{\text{max}}}\\)                                 |\\(1.5\\)                 |10\\(\\%\\) |
|\\(b\\)                        |\\(0\lt{}b\leq{}a\\)                         |\\({d\_{\text{min}}}\leq{}b\leq{}{d\_{\text{max}}}\\)                              |\\(1.2\\) \\({\text{m}}\\)|10\\(\\%\\) |
|\\({P\_{\text{b}\text{tol}}}\\)|\\(0\leq{}{P\_{\text{b}\text{tol}}}\leq{}1\\)|--                                                                                 |\\(0.008\\)               |0.1\\(\\%\\)|
|\\(\mathit{SD}\\)              |\\(\mathit{SD}\gt{}0\\)                      |\\({\mathit{SD}\_{\text{min}}}\leq{}\mathit{SD}\leq{}{\mathit{SD}\_{\text{max}}}\\)|\\(45\\) \\({\text{m}}\\) |10\\(\\%\\) |
|\\(\mathit{TNT}\\)             |\\(\mathit{TNT}\gt{}0\\)                     |--                                                                                 |\\(1\\)                   |10\\(\\%\\) |
|\\(w\\)                        |\\(w\gt{}0\\)                                |\\({w\_{\text{min}}}\leq{}w\leq{}{w\_{\text{max}}}\\)                              |\\(42\\) \\({\text{kg}}\\)|10\\(\\%\\) |

**<p align="center">Input Data Constraints</p>**
