# Introduction

Scientists and researchers frequently use ODE as a model in scientific problems, and this model describes the nature phenomenons. Drasil is a framework able to generate software artifacts that solve scientific problems. Currently, the Drasil team is interested to expand its knowledge to solve more ODEs. However, this reaches the bottleneck that the Drasil framework lacks the capability to solve high order ordinary differential equations. The previous researcher conducted research to solve the first-order ODE, but it covers a small area of the knowledge of ordinary differential equations. A first-order linear ODE and higher-order linear ODE belong to the family of a linear ode which is a new area the Drasil framework does not fully cover before. On top of that, the Drasil team has not studied a system of ODE before.

The purpose of this research is to 
- extend full capability solving a linear high order ODE numerically in Drasil
- explore the possibility of solving a system of ODE numerically (non-linear double pendulum as a case study)

# Background 
- what is Drasil
- what can Drasil do now
- how to solve a high order ODE on paper

# Chapter 1 ODE Model
- internal data represent, AX = c
    - data type
    - constructor
    - violation guard
- display matrix
- input language 

# Chapter 2 External libraries
- what are they
- similarity of how libraries solve the ODEs
- what option do libraries provide
- handle external libraries in Drasil - symbolic links

# Chapter 3 Connect Drasil with External libraries
- connect ODE Model with External libraries
    - transform matrix represent to solver format (X' = AX + c)
- upgraded interface for solving high order ODE
- manually build solve format for double pendulum (non-linear ODE example)

# Chapter 4 Add Choices
- re-structure the Choices tree diagram
- add flexibility to allow user output solutions for multiple dependent variables
- add flexibility to allow user output function
    - Python library output a genetic class
    - CSharp library output an IEnumerable collection
