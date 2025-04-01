# Data Constraints {#Sec:DataConstraints}

The [Data Constraints Table](./SecDataConstraints.md#Table:InDataConstraints) shows the data constraints on the input variables. The column for physical constraints gives the physical limitations on the range of values that can be taken by the variable. The uncertainty column provides an estimate of the confidence with which the physical quantities can be measured. This information would be part of the input if one were performing an uncertainty quantification exercise. The constraints are conservative to give the user of the model the flexibility to experiment with unusual situations. The column of typical values is intended to provide a feel for a common scenario. The column for software constraints restricts the range of inputs to reasonable values.

<div id="Table:InDataConstraints"></div>

|Var                    |Physical Constraints                              |Software Constraints                                                               |Typical Value                                                   |Uncert.    |
|:----------------------|:-------------------------------------------------|:----------------------------------------------------------------------------------|:---------------------------------------------------------------|:----------|
|\\(A\_{\text{C}}\\)    |\\(A\_{\text{C}}\gt{}0\\)                         |\\(A\_{\text{C}}\leq{}A^{\text{max}}\_{\text{C}}\\)                                |\\(0.12\\) \\({\text{m}^{2}}\\)                                 |10\\(\\%\\)|
|\\(C\_{\text{W}}\\)    |\\(C\_{\text{W}}\gt{}0\\)                         |\\(C^{\text{min}}\_{\text{W}}\lt{}C\_{\text{W}}\lt{}C^{\text{max}}\_{\text{W}}\\)  |\\(4186\\) \\(\frac{\text{J}}{\text{kg}{}^{\circ}\text{C}}\\)   |10\\(\\%\\)|
|\\(D\\)                |\\(D\gt{}0\\)                                     |\\(\mathit{AR}\_{\text{min}}\leq{}D\leq{}\mathit{AR}\_{\text{max}}\\)              |\\(0.412\\) \\({\text{m}}\\)                                    |10\\(\\%\\)|
|\\(h\_{\text{C}}\\)    |\\(h\_{\text{C}}\gt{}0\\)                         |\\(h^{\text{min}}\_{\text{C}}\leq{}h\_{\text{C}}\leq{}h^{\text{max}}\_{\text{C}}\\)|\\(1000\\) \\(\frac{\text{W}}{\text{m}^{2}{}^{\circ}\text{C}}\\)|10\\(\\%\\)|
|\\(L\\)                |\\(L\gt{}0\\)                                     |\\(L\_{\text{min}}\leq{}L\leq{}L\_{\text{max}}\\)                                  |\\(1.5\\) \\({\text{m}}\\)                                      |10\\(\\%\\)|
|\\(T\_{\text{C}}\\)    |\\(0\lt{}T\_{\text{C}}\lt{}100\\)                 |--                                                                                 |\\(50\\) \\({{}^{\circ}\text{C}}\\)                             |10\\(\\%\\)|
|\\(T\_{\text{init}}\\) |\\(0\lt{}T\_{\text{init}}\lt{}100\\)              |--                                                                                 |\\(40\\) \\({{}^{\circ}\text{C}}\\)                             |10\\(\\%\\)|
|\\(t\_{\text{final}}\\)|\\(t\_{\text{final}}\gt{}0\\)                     |\\(t\_{\text{final}}\lt{}t^{\text{max}}\_{\text{final}}\\)                         |\\(50000\\) \\({\text{s}}\\)                                    |10\\(\\%\\)|
|\\(t\_{\text{step}}\\) |\\(0\lt{}t\_{\text{step}}\lt{}t\_{\text{final}}\\)|--                                                                                 |\\(0.01\\) \\({\text{s}}\\)                                     |10\\(\\%\\)|
|\\(ρ\_{\text{W}}\\)    |\\(ρ\_{\text{W}}\gt{}0\\)                         |\\(ρ^{\text{min}}\_{\text{W}}\lt{}ρ\_{\text{W}}\leq{}ρ^{\text{max}}\_{\text{W}}\\) |\\(1000\\) \\(\frac{\text{kg}}{\text{m}^{3}}\\)                 |10\\(\\%\\)|

**<p align="center">Input Data Constraints</p>**
