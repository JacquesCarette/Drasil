# Problem
The Drasil framework lacks the capability to solve high order differential equations

# Knowledge gap
The previous researcher conducted research to solve the first-order ode, but it covers a small area of the knowledge of ordinary differential equations. A first-order linear ODE and higher-order linear ODE belong to the family of a linear ode which is a new area the Drasil framework does not fully cover before. On top of that, the Drasil team has not studied a system of ODE before.

# Context
Scientists and researchers frequently use ODE as a model in scientific problems, and this model describes the nature phenomenons. Drasil is a framework able to generate software artifacts that solve scientific problems. Currently, the Drasil team is interested to expand its knowledge to solve more ODEs.

# Evidence 
- Brook thesis - external libraries implementation
- ODE text book
- https://tutorial.math.lamar.edu/classes/de/systemsde.aspx Example 2

# Conceptual Framework
- it is mainly the how the context is being studied and analyzed
- This research is based on previous research (Brooks). It covers the ODE knowledge which has not be covered before. 

# Purpose Statement
- The purpose of this research is to 
    - extend full capability solving a linear high order ODE numerically in Drasil
    - explore the possibility of solving a system of ODE numerically (non-linear double pendulum as a case study)

# Questions
1. what is drasil framework?
    - Drasil is a framework can generate software artifacts includes code, documentation, software requirement documentation (SRS), and more. 

2. what is external libraries, and what they can do?
    - Brook detail how to implement the libraries, but doesn't mentioned what the capability of those external libraries.

3. what can drasil do now?
    Users encode knowledge into a chuck. All chucks be collected in SystemInformation. Each time when Drasil generate one artifacts, it will retrieve information from SystemInformation. 

    Before the extension, Drasil only can solve a single first oder ode. There is a implementation solve a second order ode in PDcontrller case study, however, it only generates python code and it excludes most features from the original design. On top of that, to solve the first order ODE, it requires a user manually builds up equations which the ODE solvers able to recognize.

    After the extension, Drasil not only can solve any high order single ODE, but also automate generate equations which the ODE solvers able to use. 

4. what is solving ODE means?
    The solution of ODE can be divide into two main parts, initial value problem and boundary value problem.
    IVP
    Numerical Solution

5. how to solve ODE in Drasil?
    - how to solve ODE manually

    - Differential Model:
        - Input Language
        - Matrix Represent 
        - Transform matrix represent to solver format
    
    - Choice option
