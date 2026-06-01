# General Definitions

This section collects the laws and equations that will be used to build the instance models.

<div id="GD:rectVel">

|Refname|GD:rectVel|
|-|-|
|Label|Rectilinear (1D) velocity as a function of time for constant acceleration|
|Units|\\(\frac{\text{m}}{\text{s}}\\)|
|Equation|\\[v\text{(}t\text{)}={v^{\text{i}}}+{a^{c}} t\\]|
|Description|<ul><li>\\(v\text{(}t\text{)}\\) is the 1D speed (\\(\frac{\text{m}}{\text{s}}\\)) </li> <li> \\({v^{\text{i}}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\)) </li> <li> \\({a^{c}}\\) is the constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li><li> \\(t\\) is the time (\\({\text{s}}\\)) </li></ul>|
|Source|[hibbeler2004](./references.md#hibbeler2004) (pg. 8)|
|RefBy|[GD:velVec](./general-definitions.md#GD:velVec) and [GD:rectPos](./general-definitions.md#GD:rectPos)|

</div>

#### Detailed derivation of rectilinear velocity:

Assume we have rectilinear motion of a particle (of negligible size and shape, from [A:pointMass](./assumptions.md#pointMass)); that is, motion in a straight line. The velocity is \\(v\\) and the acceleration is \\(a\\). The motion in [TM:acceleration](./theoretical-models.md#TM:acceleration) is now one-dimensional with a constant acceleration, represented by \\({a^{c}}\\). The initial velocity (at \\(t = 0\\), from [A:timeStartZero](./assumptions.md#timeStartZero)) is represented by \\({v^{\text{i}}}\\). [TM:acceleration](./theoretical-models.md#TM:acceleration) in 1D, and using the above symbols we have:

\\[{a^{c}}=\frac{\\,dv}{\\,dt}\\]

Rearranging and integrating, we have:

\\[\int_{{v^{\text{i}}}}^{v}\\,{1}\\,dv=\int_{0}^{t}\\,{{a^{c}}}\\,dt\\]

Performing the integration, we have the required equation:

\\[v\text{(}t\text{)}={v^{\text{i}}}+{a^{c}} t\\]

</br>

<div id="GD:rectPos">

|Refname|GD:rectPos|
|-|-|
|Label|Rectilinear (1D) position as a function of time for constant acceleration|
|Units|\\({\text{m}}\\)|
|Equation|\\[p\text{(}t\text{)}={p^{\text{i}}}+{v^{\text{i}}} t+\frac{{a^{c}} t^{2}}{2}\\]|
|Description|<ul><li>\\(p\text{(}t\text{)}\\) is the 1D position (\\({\text{m}}\\)) </li> <li> \\({p^{\text{i}}}\\) is the initial position (\\({\text{m}}\\)) </li><li> \\({v^{\text{i}}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\(t\\) is the time (\\({\text{s}}\\)) </li><li> \\({a^{c}}\\) is the constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li></ul>|
|Source|[hibbeler2004](./references.md#hibbeler2004) (pg. 8)|
|RefBy|[GD:posVec](./general-definitions.md#GD:posVec)|

</div>

#### Detailed derivation of rectilinear position:

Assume we have rectilinear motion of a particle (of negligible size and shape, from [A:pointMass](./assumptions.md#pointMass)); that is, motion in a straight line. The position is \\(p\\) and the velocity is \\(v\\). The motion in [TM:velocity](./theoretical-models.md#TM:velocity) is now one-dimensional. The initial position (at \\(t=0\\), from [A:timeStartZero](./assumptions.md#timeStartZero)) is represented by \\({p^{\text{i}}}\\). From [TM:velocity](./theoretical-models.md#TM:velocity) in 1D, and using the above symbols we have:

\\[v=\frac{\\,dp}{\\,dt}\\]

Rearranging and integrating, we have:

\\[\int_{{p^{\text{i}}}}^{p}{1}\\,dp=\int_{0}^{t}{v}\\,dt\\]

From [GD:rectVel](./general-definitions.md#GD:rectVel), we can replace \\(v\\):

\\[\int_{{p^{\text{i}}}}^{p}{1}\\,dp=\int_{0}^{t}{{v^{\text{i}}}+{a^{c}} t}\\,dt\\]

Performing the integration, we have the required equation:

\\[p\text{(}t\text{)}={p^{\text{i}}}+{v^{\text{i}}} t+\frac{{a^{c}} t^{2}}{2}\\]

</br>

<div id="GD:velVec">

|Refname|GD:velVec|
|-|-|
|Label|Velocity vector as a function of time for 2D motion under constant acceleration|
|Units|\\(\frac{\text{m}}{\text{s}}\\)|
|Equation|\\[\boldsymbol{v}\text{(}t\text{)}=\begin{bmatrix}{{v_{\text{x}}}^{\text{i}}}+{{a_{\text{x}}}^{\text{c}}} t\\\ {{v_{\text{y}}}^{\text{i}}}+{{a_{\text{y}}}^{\text{c}}} t\end{bmatrix}\\]|
|Description|<ul><li> \\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({{v_{\text{x}}}^{\text{i}}}\\) is the \\(x\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({{a_{\text{x}}}^{\text{c}}}\\) is the \\(x\\)-component of constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li><li> \\(t\\) is the time (\\({\text{s}}\\)) </li><li> \\({{v_{\text{y}}}^{\text{i}}}\\) is the \\(y\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({{a_{\text{y}}}^{\text{c}}}\\) is the \\(y\\)-component of constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li></ul>|
|Source|--|
|RefBy||

</div>

#### Detailed derivation of velocity vector:

For a two-dimensional Cartesian coordinate system ([A:twoDMotion](./assumptions.md#twoDMotion) and [A:cartSyst](./assumptions.md#cartSyst)), we can represent the velocity vector as \\(\boldsymbol{v}\text{(}t\text{)}=\begin{bmatrix}{v_{\text{x}}}\\\ {v_{\text{y}}}\end{bmatrix}\\) and the acceleration vector as \\(\boldsymbol{a}\text{(}t\text{)}=\begin{bmatrix}{a_{\text{x}}}\\\ {a_{\text{y}}}\end{bmatrix}\\). The acceleration is assumed to be constant ([A:constAccel](./assumptions.md#constAccel)) and the constant acceleration vector is represented as \\({\boldsymbol{a}^{\text{c}}}=\begin{bmatrix}{{a_{\text{x}}}^{\text{c}}}\\\ {{a_{\text{y}}}^{\text{c}}}\end{bmatrix}\\). The initial velocity (at \\(t=0\\), from [A:timeStartZero](./assumptions.md#timeStartZero)) is represented by \\({\boldsymbol{v}^{\text{i}}}=\begin{bmatrix}{{v_{\text{x}}}^{\text{i}}}\\\ {{v_{\text{y}}}^{\text{i}}}\end{bmatrix}\\). Since we have a Cartesian coordinate system, [GD:rectVel](./general-definitions.md#GD:rectVel) can be applied to each coordinate of the velocity vector to yield the required equation:

\\[\boldsymbol{v}\text{(}t\text{)}=\begin{bmatrix}{{v_{\text{x}}}^{\text{i}}}+{{a_{\text{x}}}^{\text{c}}} t\\\ {{v_{\text{y}}}^{\text{i}}}+{{a_{\text{y}}}^{\text{c}}} t\end{bmatrix}\\]

</br>

<div id="GD:posVec">

|Refname|GD:posVec|
|-|-|
|Label|Position vector as a function of time for 2D motion under constant acceleration|
|Units|\\(m\\)|
|Equation|\\[\boldsymbol{p}\text{(}t\text{)}=\begin{bmatrix}{{p_{\text{x}}}^{\text{i}}}+{{v_{\text{x}}}^{\text{i}}} t+\frac{{{a_{\text{x}}}^{\text{c}}} t^{2}}{2}\\\ {{p_{\text{y}}}^{\text{i}}}+{{v_{\text{y}}}^{\text{i}}} t+\frac{{{a_{\text{y}}}^{\text{c}}} t^{2}}{2}\end{bmatrix}\\]|
|Description|<ul><li> \\(\boldsymbol{p}\text{(}t\text{)}\\) is the position (\\({\text{m}}\\)) <li> \\({{p_{\text{x}}}^{\text{i}}}\\) is the \\(x\\)-component of initial position (\\({\text{m}}\\)) </li><li> \\({{v_{\text{x}}}^{\text{i}}}\\) is the \\(x\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\(t\\) is the time (\\({\text{s}}\\)) </li><li> \\({{a_{\text{x}}}^{\text{c}}}\\) is the \\(x\\)-component of constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li><li> \\({{p_{\text{y}}}^{\text{i}}}\\) is the \\(y\\)-component of initial position (\\({\text{m}}\\)) </li><li> \\({{v_{\text{y}}}^{\text{i}}}\\) is the \\(y\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({{a_{\text{y}}}^{\text{c}}}\\) is the \\(y\\)-component of constant acceleration (\\(\frac{\text{m}}{\text{s}^{2}}\\)) </li></ul>|
|Source|--|
|RefBy|[IM:calOfLandingDist](./instance-models.md#IM:calOfLandingDist) and [IM:calOfLandingTime](./instance-models.md#IM:calOfLandingTime)|

</div>

#### Detailed derivation of position vector:

For a two-dimensional Cartesian coordinate system ([A:twoDMotion](./assumptions.md#twoDMotion) and [A:cartSyst](./assumptions.md#cartSyst)), we can represent the position vector as \\(\boldsymbol{p}\text{(}t\text{)}=\begin{bmatrix}{p_{\text{x}}}\\\ {p_{\text{y}}}\end{bmatrix}\\), the velocity vector as \\(\boldsymbol{v}\text{(}t\text{)}=\begin{bmatrix}{v_{\text{x}}}\\\ {v_{\text{y}}}\end{bmatrix}\\), and the acceleration vector as \\(\boldsymbol{a}\text{(}t\text{)}=\begin{bmatrix}{a_{\text{x}}}\\\ {a_{\text{y}}}\end{bmatrix}\\). The acceleration is assumed to be constant ([A:constAccel](./assumptions.md#constAccel)) and the constant acceleration vector is represented as \\({\boldsymbol{a}^{\text{c}}}=\begin{bmatrix}{{a_{\text{x}}}^{\text{c}}}\\\ {{a_{\text{y}}}^{\text{c}}}\end{bmatrix}\\). The initial velocity (at \\(t=0\\), from [A:timeStartZero](./assumptions.md#timeStartZero)) is represented by \\({\boldsymbol{v}^{\text{i}}}=\begin{bmatrix}{{v_{\text{x}}}^{\text{i}}}\\\ {{v_{\text{y}}}^{\text{i}}}\end{bmatrix}\\). Since we have a Cartesian coordinate system, [GD:rectPos](./general-definitions.md#GD:rectPos) can be applied to each coordinate of the position vector to yield the required equation:

\\[\boldsymbol{p}\text{(}t\text{)}=\begin{bmatrix}{{p_{\text{x}}}^{\text{i}}}+{{v_{\text{x}}}^{\text{i}}} t+\frac{{{a_{\text{x}}}^{\text{c}}} t^{2}}{2}\\\ {{p_{\text{y}}}^{\text{i}}}+{{v_{\text{y}}}^{\text{i}}} t+\frac{{{a_{\text{y}}}^{\text{c}}} t^{2}}{2}\end{bmatrix}\\]
