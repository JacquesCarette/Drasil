# Data Definitions

This section collects and defines all the data needed to build the instance models.

<div id = "DD:vecMag">

|Refname|DD:vecMag|
|-|-|
|Label|Speed|
|Symbol|\\(v\\)|
|Units|\\(\frac{\text{m}}{\text{s}}\\)|
|Equation|\\[v=\|\boldsymbol{v}\text{(}t\text{)}\|\\]|
|Description|<ul><li> \\(v\\) is the speed (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\(\boldsymbol{v}\text{(}t\text{)}\\) is the velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li></ul>|
|Notes|For a given velocity vector \\(\boldsymbol{v}\text{(}t\text{)}\\), the magnitude of the vector (\\(\|\boldsymbol{v}\text{(}t\text{)}\|\\)) is the scalar called speed.|
|Source|--|
|RefBy|[DD:speedIY](./data-definitions.md#DD:speedIY) and [DD:speedIX](./data-definitions.md#DD:speedIX)|

</div>

</br>

<div id="DD:speedIX">

|Refname|DD:speedIX|
|-|-|
|Label|\\(x\\)-component of initial velocity|
|Symbol|\\({{v_{\text{x}}}^{\text{i}}}\\)|
|Units|\\(\frac{\text{m}}{\text{s}}\\)|
|Equation|\\[{{v_{\text{x}}}^{\text{i}}}={v^{\text{i}}} \cos\left(θ\right)\\]|
|Description|<ul><li> \\({{v_{\text{x}}}^{\text{i}}}\\) is the \\(x\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({v^{\text{i}}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\(θ\\) is the launch angle (\\({\text{rad}}\\))|
|Notes|<ul><li> \\({v^{\text{i}}}\\) is from [DD:vecMag](./data-definitions.md#DD:vecMag). </li><li> \\(θ\\) is shown in [Fig:Launch](./physical-sys-description.md#Fig:Launch).|
|Source|--|
|RefBy|[IM:calOfLandingDist](./instance-models.md#IM:calOfLandingDist)|

</div>

</br>

<div id="DD:speedIY">

|Refname|DD:speedIY|
|-|-|
|Label|\\(y\\)-component of initial velocity|
|Symbol|\\({{v_{\text{y}}}^{\text{i}}}\\)|
|Units|\\(\frac{\text{m}}{\text{s}}\\)|
|Equation|\\[{{v_{\text{y}}}^{\text{i}}}={v^{\text{i}}} \sin\left(θ\right)\\]|
|Description|<ul><li> \\({{v_{\text{y}}}^{\text{i}}}\\) is the \\(y\\)-component of initial velocity (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\({v^{\text{i}}}\\) is the initial speed (\\(\frac{\text{m}}{\text{s}}\\)) </li><li> \\(θ\\) is the launch angle (\\({\text{rad}}\\))|
|Notes|<ul><li> \\({v^{\text{i}}}\\) is from [DD:vecMag](./data-definitions.md#data-definitions#DD:vecMag). </li><li> \\(θ\\) is shown in [Figure:Launch](./physical-sys-description.md#Figure:Launch). </li></ul>|
|Source|--|
|RefBy|[IM:calOfLandingTime](./instance-models.md#IM:calOfLandingTime)|

</div>
