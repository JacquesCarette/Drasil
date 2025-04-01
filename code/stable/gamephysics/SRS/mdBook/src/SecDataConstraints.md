# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario.

<div id="Table:InDataConstraints"></div>

|Var                                  |Physical Constraints               |Software Constraints     |Typical Value                                                               |Uncert.    |
|:------------------------------------|:----------------------------------|:------------------------|:---------------------------------------------------------------------------|:----------|
|\\({C\_{\text{R}}}\\)                |\\(0\leq{}{C\_{\text{R}}}\leq{}1\\)|--                       |\\(0.8\\)                                                                   |10\\(\\%\\)|
|\\(\boldsymbol{F}\\)                 |--                                 |--                       |\\(98.1\\) \\({\text{N}}\\)                                                 |10\\(\\%\\)|
|\\(G\\)                              |--                                 |--                       |\\(66.743\cdot{}10^{-12}\\) \\(\frac{\text{m}^{3}}{\text{kg}\text{s}^{2}}\\)|10\\(\\%\\)|
|\\(\boldsymbol{I}\\)                 |\\(\boldsymbol{I}\gt{}0\\)         |--                       |\\(74.5\\) \\(\text{kg}\text{m}^{2}\\)                                      |10\\(\\%\\)|
|\\(L\\)                              |\\(L\gt{}0\\)                      |--                       |\\(44.2\\) \\({\text{m}}\\)                                                 |10\\(\\%\\)|
|\\(m\\)                              |\\(m\gt{}0\\)                      |--                       |\\(56.2\\) \\({\text{kg}}\\)                                                |10\\(\\%\\)|
|\\(\boldsymbol{p}\text{(}t\text{)}\\)|--                                 |--                       |\\(0.412\\) \\({\text{m}}\\)                                                |10\\(\\%\\)|
|\\(\boldsymbol{v}\text{(}t\text{)}\\)|--                                 |--                       |\\(2.51\\) \\(\frac{\text{m}}{\text{s}}\\)                                  |10\\(\\%\\)|
|\\(\boldsymbol{τ}\\)                 |--                                 |--                       |\\(200\\) \\(\text{N}\text{m}\\)                                            |10\\(\\%\\)|
|\\(ω\\)                              |--                                 |--                       |\\(2.1\\) \\(\frac{\text{rad}}{\text{s}}\\)                                 |10\\(\\%\\)|
|\\(ϕ\\)                              |--                                 |\\(0\leq{}ϕ\leq{}2\\,π\\)|\\(\frac{π}{2}\\) \\({\text{rad}}\\)                                        |10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**
