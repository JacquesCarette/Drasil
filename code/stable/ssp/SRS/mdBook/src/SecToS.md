# Table of Symbols {#Sec:ToS}

The symbols used in this document are summarized in the [Table of Symbols](./SecToS.md#Table:ToS) along with their units. Throughout the document, a subscript \\(i\\) indicates that the value will be taken at, and analyzed at, a slice or slice interface composing the total slip mass. For vector quantities, the units shown are for each component of the vector.

<div id="Table:ToS"></div>

|Symbol                                                                  |Description                                                                                                                                                                                                     |Units                               |
|:-----------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------|
|\\(A\\)                                                                 |Area: A part of an object or surface.                                                                                                                                                                           |\\({\text{m}^{2}}\\)                |
|\\(\boldsymbol{a}\text{(}t\text{)}\\)                                   |Acceleration: The rate of change of a body's velocity.                                                                                                                                                          |\\(\frac{\text{m}}{\text{s}^{2}}\\) |
|\\(\boldsymbol{b}\\)                                                    |Base Width of Slices: The width of each slice in the \\(x\\)-direction.                                                                                                                                         |\\({\text{m}}\\)                    |
|\\({\boldsymbol{C}\_{\text{den}}}\\)                                    |Proportionality Constant Denominator: Values for each slice that sum together to form the denominator of the interslice normal to shear force proportionality constant.                                         |\\({\text{N}}\\)                    |
|\\({\boldsymbol{C}\_{\text{num}}}\\)                                    |Proportionality Constant Numerator: Values for each slice that sum together to form the numerator of the interslice normal to shear force proportionality constant.                                             |\\({\text{N}}\\)                    |
|\\(c'\\)                                                                |Effective Cohesion: The internal pressure that sticks particles of soil together.                                                                                                                               |\\({\text{Pa}}\\)                   |
|\\(\mathit{const\_f}\\)                                                 |Decision on F: A Boolean decision on which form of f the user desires: constant if true, or half-sine if false.                                                                                                 |--                                  |
|\\({F\_{\text{n}}}\\)                                                   |Total Normal Force: Component of a force in the normal direction.                                                                                                                                               |\\({\text{N}}\\)                    |
|\\({F\_{\text{rot}}}\\)                                                 |Force Causing Rotation: A force in the direction of rotation.                                                                                                                                                   |\\({\text{N}}\\)                    |
|\\({F\_{\text{S}}}\\)                                                   |Factor of Safety: The global stability metric of a slip surface of a slope, defined as the ratio of resistive shear force to mobilized shear force.                                                             |--                                  |
|\\({F\_{\text{t}}}\\)                                                   |Tangential Force: Component of a force in the tangential direction.                                                                                                                                             |\\({\text{N}}\\)                    |
|\\({F\_{\text{x}}}\\)                                                   |\\(x\\)-coordinate of the Force: The force acting in the \\(x\\)-direction.                                                                                                                                     |\\({\text{N}}\\)                    |
|\\({F\_{\text{y}}}\\)                                                   |\\(y\\)-coordinate of the Force: The force acting in the \\(y\\)-direction.                                                                                                                                     |\\({\text{N}}\\)                    |
|\\(\boldsymbol{F}\\)                                                    |Force: An interaction that tends to produce change in the motion of an object.                                                                                                                                  |\\({\text{N}}\\)                    |
|\\({{\boldsymbol{F}\_{\text{x}}}^{\text{G}}}\\)                         |Sums of the Interslice Normal Forces: The sums of the normal forces acting on each pair of adjacent interslice boundaries.                                                                                      |\\({\text{N}}\\)                    |
|\\({{\boldsymbol{F}\_{\text{x}}}^{\text{H}}}\\)                         |Sums of the Interslice Normal Water Forces: The sums of the normal water forces acting on each pair of adjacent interslice boundaries.                                                                          |\\({\text{N}}\\)                    |
|\\(\boldsymbol{f}\\)                                                    |Interslice Normal to Shear Force Ratio Variation Function: A function of distance in the \\(x\\)-direction that describes the variation of the interslice normal to shear ratio.                                |--                                  |
|\\(\boldsymbol{G}\\)                                                    |Interslice Normal Forces: The forces per meter in the \\(z\\)-direction exerted between each pair of adjacent slices.                                                                                           |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(\boldsymbol{g}\\)                                                    |Gravitational Acceleration: The approximate acceleration due to gravity on Earth at sea level.                                                                                                                  |\\(\frac{\text{m}}{\text{s}^{2}}\\) |
|\\(\boldsymbol{H}\\)                                                    |Interslice Normal Water Forces: The normal water forces per meter in the \\(z\\)-direction exerted in the \\(x\\)-direction between each pair of adjacent slices.                                               |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(h\\)                                                                 |Height: The distance above a reference point for a point of interest.                                                                                                                                           |\\({\text{m}}\\)                    |
|\\(\boldsymbol{h}\\)                                                    |\\(y\\)-direction Heights of Slices: The heights in the \\(y\\)-direction from the base of each slice to the slope surface, at the \\(x\\)-direction midpoint of the slice.                                     |\\({\text{m}}\\)                    |
|\\({\boldsymbol{h}^{\text{L}}}\\)                                       |Heights of the Left Side of Slices: The heights of the left side of each slice, assuming slice surfaces have negative slope.                                                                                    |\\({\text{m}}\\)                    |
|\\({\boldsymbol{h}^{\text{R}}}\\)                                       |Heights of the Right Side of Slices: The heights of the right side of each slice, assuming slice surfaces have negative slope.                                                                                  |\\({\text{m}}\\)                    |
|\\({\boldsymbol{h}\_{\text{z}}}\\)                                      |Heights of Interslice Normal Forces: The heights in the \\(y\\)-direction of the interslice normal forces on each slice.                                                                                        |\\({\text{m}}\\)                    |
|\\({\boldsymbol{h}\_{\text{z,w}}}\\)                                    |Heights of the Water Table: The heights in the \\(y\\)-direction from the base of each slice to the water table.                                                                                                |\\({\text{m}}\\)                    |
|\\(i\\)                                                                 |Index: A number representing a single slice.                                                                                                                                                                    |--                                  |
|\\(\boldsymbol{\hat{j}}\\)                                              |Unit Vector: A vector that has a magnitude of one.                                                                                                                                                              |--                                  |
|\\({K\_{\text{c}}}\\)                                                   |Seismic Coefficient: The proportionality factor of force that weight pushes outwards; caused by seismic earth movements.                                                                                        |--                                  |
|\\({\boldsymbol{L}\_{b}}\\)                                             |Total Base Lengths of Slices: The lengths of each slice in the direction parallel to the slope of the base.                                                                                                     |\\({\text{m}}\\)                    |
|\\({\boldsymbol{L}\_{s}}\\)                                             |Surface Lengths of Slices: The lengths of each slice in the direction parallel to the slope of the surface.                                                                                                     |\\({\text{m}}\\)                    |
|\\(M\\)                                                                 |Moment: A measure of the tendency of a body to rotate about a specific point or axis.                                                                                                                           |\\(\text{N}\text{m}\\)              |
|\\(m\\)                                                                 |Mass: The quantity of matter in a body.                                                                                                                                                                         |\\({\text{kg}}\\)                   |
|\\(\boldsymbol{N}\\)                                                    |Normal Forces: The total reactive forces per meter in the \\(z\\)-direction for each slice of a soil surface subject to a body resting on it.                                                                   |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(\boldsymbol{N'}\\)                                                   |Effective Normal Forces: The forces per meter in the \\(z\\)-direction for each slice of a soil surface, subtracting pore water reactive force from total reactive force.                                       |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(n\\)                                                                 |Number of Slices: The number of slices into which the slip surface is divided.                                                                                                                                  |--                                  |
|\\(P\\)                                                                 |Resistive Shear Force: The Mohr Coulomb frictional force that describes the limit of mobilized shear force that can be withstood before failure.                                                                |\\({\text{N}}\\)                    |
|\\(\boldsymbol{P}\\)                                                    |Resistive Shear Forces: The Mohr Coulomb frictional forces per meter in the \\(z\\)-direction for each slice that describe the limit of mobilized shear force the slice can withstand before failure.           |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(p\\)                                                                 |Pressure: A force exerted over an area.                                                                                                                                                                         |\\({\text{Pa}}\\)                   |
|\\(\boldsymbol{Q}\\)                                                    |External Forces: The forces per meter in the \\(z\\)-direction acting into the surface from the midpoint of each slice.                                                                                         |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(\boldsymbol{R}\\)                                                    |Resistive Shear Forces Without the Influence of Interslice Forces: The resistive shear forces per meter without the influence of interslice forces in the \\(z\\)-direction for each slice.                     |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(r\\)                                                                 |Length of the Moment Arm: The distance between a force causing rotation and the axis of rotation.                                                                                                               |\\({\text{m}}\\)                    |
|\\(\boldsymbol{r}\\)                                                    |Position Vector: A vector from the origin of the Cartesian coordinate system defined to the point where the force is applied.                                                                                   |\\({\text{m}}\\)                    |
|\\(S\\)                                                                 |Mobilized Shear Force: The shear force in the direction of potential motion.                                                                                                                                    |\\({\text{N}}\\)                    |
|\\(\boldsymbol{S}\\)                                                    |Mobilized Shear Force: The mobilized shear force per meter in the \\(z\\)-direction for each slice.                                                                                                             |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(\boldsymbol{T}\\)                                                    |Mobilized Shear Forces Without the Influence of Interslice Forces: The mobilized shear forces per meter without the influence of interslice forces in the \\(z\\)-direction for each slice.                     |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\({\boldsymbol{U}\_{\text{b}}}\\)                                      |Base Hydrostatic Forces: The forces per meter in the \\(z\\)-direction from water pressure within each slice.                                                                                                   |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\({\boldsymbol{U}\_{\text{g}}}\\)                                      |Surface Hydrostatic Forces: The forces per meter in the \\(z\\)-direction from water pressure acting into each slice from standing water on the slope surface.                                                  |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(u\\)                                                                 |Pore Pressure: The pressure that comes from water within the soil.                                                                                                                                              |\\({\text{Pa}}\\)                   |
|\\(\boldsymbol{u}\\)                                                    |Displacement: The change in an object's location relative to a reference point.                                                                                                                                 |\\({\text{m}}\\)                    |
|\\(V\\)                                                                 |Volume: The amount of space that a substance or object occupies.                                                                                                                                                |\\({\text{m}^{3}}\\)                |
|\\({\boldsymbol{V}\_{\text{dry}}}\\)                                    |Volumes of Dry Soil: The amount of space occupied by dry soil for each slice.                                                                                                                                   |\\({\text{m}^{3}}\\)                |
|\\({\boldsymbol{V}\_{\text{sat}}}\\)                                    |Volumes of Saturated Soil: The amount of space occupied by saturated soil for each slice.                                                                                                                       |\\({\text{m}^{3}}\\)                |
|\\(v\\)                                                                 |Local Index: Used as a bound variable index in calculations.                                                                                                                                                    |--                                  |
|\\(W\\)                                                                 |Weight: The gravitational force acting on an object.                                                                                                                                                            |\\({\text{N}}\\)                    |
|\\(\boldsymbol{W}\\)                                                    |Weights: The downward force per meter in the \\(z\\)-direction on each slice caused by gravity.                                                                                                                 |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(\boldsymbol{X}\\)                                                    |Interslice Shear Forces: The shear forces per meter in the \\(z\\)-direction exerted between adjacent slices.                                                                                                   |\\(\frac{\text{N}}{\text{m}}\\)     |
|\\(x\\)                                                                 |\\(x\\)-coordinate: The \\(x\\)-coordinate in the Cartesian coordinate system.                                                                                                                                  |\\({\text{m}}\\)                    |
|\\({{x\_{\text{slip}}}^{\text{maxEtr}}}\\)                              |Maximum Entry \\(x\\)-coordinate: The maximum potential \\(x\\)-coordinate for the entry point of a slip surface.                                                                                               |\\({\text{m}}\\)                    |
|\\({{x\_{\text{slip}}}^{\text{maxExt}}}\\)                              |Maximum Exit \\(x\\)-coordinate: The maximum potential \\(x\\)-coordinate for the exit point of a slip surface.                                                                                                 |\\({\text{m}}\\)                    |
|\\({{x\_{\text{slip}}}^{\text{minEtr}}}\\)                              |Minimum Entry \\(x\\)-coordinate: The minimum potential \\(x\\)-coordinate for the entry point of a slip surface.                                                                                               |\\({\text{m}}\\)                    |
|\\({{x\_{\text{slip}}}^{\text{minExt}}}\\)                              |Minimum Exit \\(x\\)-coordinate: The minimum potential \\(x\\)-coordinate for the exit point of a slip surface.                                                                                                 |\\({\text{m}}\\)                    |
|\\({\boldsymbol{x}\_{\text{cs}}}\text{,}{\boldsymbol{y}\_{\text{cs}}}\\)|Critical Slip Surface Coordinates: The set of \\(x\\)-coordinates and \\(y\\)-coordinates that describe the vertices of the critical slip surface.                                                              |\\({\text{m}}\\)                    |
|\\({\boldsymbol{x}\_{\text{slip}}}\\)                                   |\\(x\\)-coordinates of the Slip Surface: \\(x\\)-coordinates of points on the slip surface.                                                                                                                     |\\({\text{m}}\\)                    |
|\\({\boldsymbol{x}\_{\text{slope}}}\\)                                  |\\(x\\)-coordinates of the Slope: \\(x\\)-coordinates of points on the soil slope.                                                                                                                              |\\({\text{m}}\\)                    |
|\\({\boldsymbol{x}\_{\text{wt}}}\\)                                     |\\(x\\)-coordinates of the Water Table: X-positions of the water table.                                                                                                                                         |\\({\text{m}}\\)                    |
|\\(y\\)                                                                 |\\(y\\)-coordinate: The \\(y\\)-coordinate in the Cartesian coordinate system.                                                                                                                                  |\\({\text{m}}\\)                    |
|\\({{y\_{\text{slip}}}^{\text{max}}}\\)                                 |Maximum \\(y\\)-coordinate: The maximum potential \\(y\\)-coordinate of a point on a slip surface.                                                                                                              |\\({\text{m}}\\)                    |
|\\({{y\_{\text{slip}}}^{\text{min}}}\\)                                 |Minimum \\(y\\)-coordinate: The minimum potential \\(y\\)-coordinate of a point on a slip surface.                                                                                                              |\\({\text{m}}\\)                    |
|\\({\boldsymbol{y}\_{\text{slip}}}\\)                                   |\\(y\\)-coordinates of the Slip Surface: Heights of the slip surface.                                                                                                                                           |\\({\text{m}}\\)                    |
|\\({\boldsymbol{y}\_{\text{slope}}}\\)                                  |\\(y\\)-coordinates of the Slope: \\(y\\)-coordinates of points on the soil slope.                                                                                                                              |\\({\text{m}}\\)                    |
|\\({\boldsymbol{y}\_{\text{wt}}}\\)                                     |\\(y\\)-coordinates of the Water Table: Heights of the water table.                                                                                                                                             |\\({\text{m}}\\)                    |
|\\(z\\)                                                                 |\\(z\\)-coordinate: The \\(z\\)-coordinate in the Cartesian coordinate system.                                                                                                                                  |\\({\text{m}}\\)                    |
|\\(\boldsymbol{α}\\)                                                    |Base Angles: The angles between the base of each slice and the horizontal.                                                                                                                                      |\\({{}^{\circ}}\\)                  |
|\\(\boldsymbol{β}\\)                                                    |Surface Angles: The angles between the surface of each slice and the horizontal.                                                                                                                                |\\({{}^{\circ}}\\)                  |
|\\(γ\\)                                                                 |Specific Weight: The weight per unit volume.                                                                                                                                                                    |\\(\frac{\text{N}}{\text{m}^{3}}\\) |
|\\({γ\_{\text{dry}}}\\)                                                 |Soil Dry Unit Weight: The weight of a dry soil/ground layer divided by the volume of the layer.                                                                                                                 |\\(\frac{\text{N}}{\text{m}^{3}}\\) |
|\\({γ\_{\text{sat}}}\\)                                                 |Soil Saturated Unit Weight: The weight of saturated soil/ground layer divided by the volume of the layer.                                                                                                       |\\(\frac{\text{N}}{\text{m}^{3}}\\) |
|\\({γ\_{w}}\\)                                                          |Unit Weight of Water: The weight of one cubic meter of water.                                                                                                                                                   |\\(\frac{\text{N}}{\text{m}^{3}}\\) |
|\\(λ\\)                                                                 |Proportionality Constant: The ratio of the interslice normal to the interslice shear force.                                                                                                                     |--                                  |
|\\(π\\)                                                                 |Ratio of Circumference to Diameter for Any Circle: The ratio of a circle's circumference to its diameter.                                                                                                       |--                                  |
|\\(ρ\\)                                                                 |Density: The mass per unit volume.                                                                                                                                                                              |\\(\frac{\text{kg}}{\text{m}^{3}}\\)|
|\\(σ\\)                                                                 |Total Normal Stress: The total force per area acting on the soil mass.                                                                                                                                          |\\({\text{Pa}}\\)                   |
|\\(σ'\\)                                                                |Effective Stress: The stress in a soil mass that is effective in causing volume changes and mobilizes the shear strength arising from friction; represents the average stress carried by the soil skeleton.     |\\({\text{Pa}}\\)                   |
|\\({σ\_{N}}'\\)                                                         |Effective Normal Stress: The normal stress in a soil mass that is effective in causing volume changes; represents the average normal stress carried by the soil skeleton.                                       |\\({\text{Pa}}\\)                   |
|\\(τ\\)                                                                 |Tangential Stress: The shear force per unit area.                                                                                                                                                               |\\({\text{Pa}}\\)                   |
|\\({τ^{\text{f}}}\\)                                                    |Shear Strength: The strength of a material against shear failure.                                                                                                                                               |\\({\text{Pa}}\\)                   |
|\\(\boldsymbol{τ}\\)                                                    |Torque: A twisting force that tends to cause rotation.                                                                                                                                                          |\\(\text{N}\text{m}\\)              |
|\\(\boldsymbol{Φ}\\)                                                    |First Function for Incorporating Interslice Forces Into Shear Force: The function for converting resistive shear without the influence of interslice forces, to a calculation considering the interslice forces.|--                                  |
|\\(φ'\\)                                                                |Effective Angle of Friction: The angle of inclination with respect to the horizontal axis of the Mohr-Coulomb shear resistance line.                                                                            |\\({{}^{\circ}}\\)                  |
|\\(\boldsymbol{Ψ}\\)                                                    |Second Function for Incorporating Interslice Forces Into Shear Force: The function for converting mobile shear without the influence of interslice forces, to a calculation considering the interslice forces.  |--                                  |
|\\(\boldsymbol{ω}\\)                                                    |Imposed Load Angles: The angles between the external force acting into the surface of each slice and the vertical.                                                                                              |\\({{}^{\circ}}\\)                  |

**<p align="center">Table of Symbols</p>**