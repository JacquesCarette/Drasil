# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario.

<div id="Table:InDataConstraints"></div>

|Var                                       |Physical Constraints         |Typical Value                                  |Uncert.    |
|:-----------------------------------------|:----------------------------|:----------------------------------------------|:----------|
|\\(c'\\)                                  |\\(c'\gt{}0\\)               |\\(10000\\) \\({\text{Pa}}\\)                  |10\\(\\%\\)|
|\\({{x\_{\text{slip}}}^{\text{maxEtr}}}\\)|--                           |\\(20\\) \\({\text{m}}\\)                      |10\\(\\%\\)|
|\\({{x\_{\text{slip}}}^{\text{maxExt}}}\\)|--                           |\\(100\\) \\({\text{m}}\\)                     |10\\(\\%\\)|
|\\({{x\_{\text{slip}}}^{\text{minEtr}}}\\)|--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({{x\_{\text{slip}}}^{\text{minExt}}}\\)|--                           |\\(50\\) \\({\text{m}}\\)                      |10\\(\\%\\)|
|\\({\boldsymbol{x}\_{\text{slope}}}\\)    |--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({\boldsymbol{x}\_{\text{wt}}}\\)       |--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({{y\_{\text{slip}}}^{\text{max}}}\\)   |--                           |\\(30\\) \\({\text{m}}\\)                      |10\\(\\%\\)|
|\\({{y\_{\text{slip}}}^{\text{min}}}\\)   |--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({\boldsymbol{y}\_{\text{slope}}}\\)    |--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({\boldsymbol{y}\_{\text{wt}}}\\)       |--                           |\\(0\\) \\({\text{m}}\\)                       |10\\(\\%\\)|
|\\({γ\_{\text{dry}}}\\)                   |\\({γ\_{\text{dry}}}\gt{}0\\)|\\(20000\\) \\(\frac{\text{N}}{\text{m}^{3}}\\)|10\\(\\%\\)|
|\\({γ\_{\text{sat}}}\\)                   |\\({γ\_{\text{sat}}}\gt{}0\\)|\\(20000\\) \\(\frac{\text{N}}{\text{m}^{3}}\\)|10\\(\\%\\)|
|\\({γ\_{w}}\\)                            |\\({γ\_{w}}\gt{}0\\)         |\\(9800\\) \\(\frac{\text{N}}{\text{m}^{3}}\\) |10\\(\\%\\)|
|\\(φ'\\)                                  |\\(0\lt{}φ'\lt{}90\\)        |\\(25\\) \\({{}^{\circ}}\\)                    |10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**

