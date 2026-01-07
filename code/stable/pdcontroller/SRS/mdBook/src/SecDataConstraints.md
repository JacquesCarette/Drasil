# Data Constraints {#Sec:DataConstraints}

The [Input Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario.

<div id="Table:InDataConstraints"></div>

|Var                     |Physical Constraints                                              |Typical Value               |Uncert.    |
|:-----------------------|:-----------------------------------------------------------------|:---------------------------|:----------|
|\\({K\_{\text{d}}}\\)   |\\({K\_{\text{d}}}\geq{}0\\)                                      |\\(1\\)                     |10\\(\\%\\)|
|\\({K\_{\text{p}}}\\)   |\\({K\_{\text{p}}}\gt{}0\\)                                       |\\(20\\)                    |10\\(\\%\\)|
|\\({r\_{\text{t}}}\\)   |\\({r\_{\text{t}}}\gt{}0\\)                                       |\\(1\\)                     |10\\(\\%\\)|
|\\({t\_{\text{sim}}}\\) |\\(1\leq{}{t\_{\text{sim}}}\leq{}60\\)                            |\\(10\\) \\({\text{s}}\\)   |10\\(\\%\\)|
|\\({t\_{\text{step}}}\\)|\\(\frac{1}{1000}\leq{}{t\_{\text{step}}}\lt{}{t\_{\text{sim}}}\\)|\\(0.001\\) \\({\text{s}}\\)|10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**
