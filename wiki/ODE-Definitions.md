### Basic Knowledge
- Equations: the relation between two mathematical statements. For all free variables, if those are substituted with ground terms, and these terms are reduced to a normal form, then the lhs and rhs of the equation are identical. A ground term is a term with no free variables. A normal form is an expression that cannot be evaluated anymore.
- Derivative: a function describe the rate of change in the function value with respect to changes in its input 
    - `(change in function) / (change in input)`
- Differential equations: is an equation with an unknown function and one or more of its derivatives. Or, the relation between an unknown function and its derivatives. Or, an equation is generalized to allow for some free variables (like `x`) denoting numbers.
- The unknown function is called the dependent variable and the variable or variables on which it depends are the independent variables.
- Order: determined by the highest derivative that appears in the equation. 
- First Order: the first derivative, the only derivative that appears in the equation is a single one, with respect to a single derivative
- Second Order: the second derivative
- High Order: the `nᵗʰ` derivative, `n ∈ 𝑁`
- The highest derivative: the highest repeated steps which can be traced back to the original function by taking derivative.
- Solution: a "solution" to an ODE is a function such that, when it is "plugged in" to the equation, you reduce to "0 = 0".

### Different Differential Equations
- Ordinary Differential Equations are equations with an unknown function(only depends on a single independent variable) and one or more of its derivatives. Or, the relation between an unknown function(only depends on a single independent variable) and its derivatives.
- Partial Differential Equations are equations with an unknown function(depends on several independent variables) and one or more of its derivatives. Or, the relation between an unknown function(depends on several independent variables) and its derivatives.
- Systems of Differential Equations are multiple differential equations that describe the relationship between multiple unknown functions and their derivatives, each differential equation with multiple unknown functions and one or more of its derivatives.
  - Solving a second-order system of ODE with one dependent variable
    - In order to do it, we can reform it into two first-order odes.
      - [Example of solving a system ODE with one dependent variable](https://tutorial.math.lamar.edu/classes/de/systemsde.aspx)
      - Example in Drasil, [PDController](https://jacquescarette.github.io/Drasil/examples/pdcontroller/SRS/srs/PDController_SRS.html#Sec:IMs)
  - Solving a system of ODE with two dependent variables
    - Usually, with two dependent variables, we will have at least two DEs. To take the rising population with the number of prey and predators as an example, a system of ODEs contains two equations, and the increasing number of prey and predators depends on the current number of prey and the current number of predators. 
      - [Double pendulum, Example of solving a system ODE with two dependent variable](https://www.myphysicslab.com/pendulum/double-pendulum-en.html) Eventually, we get two second-order ODEs. In order to solve it, each second-order ode reformed to two equations, and together, we get four equations. 
      - [Solve a system ODE with two dependent variables in Python - scipy](https://scipython.com/blog/the-double-pendulum/)
      - [More examples of Solve an ODE via Python - scipy](https://apmonitor.com/pdc/index.php/Main/SolveDifferentialEquations)

### First Order Differential Equations
- First Order Differential Equations: `dy/dx = f(x, y)`, where `dy/dx` is the first order derivative, `f(x, y)` can be any function of the independent variable x and the dependent variable y. y is a variable that depends on x.

### Linear Equation
- Linear Relationship: describe a straight-line relationship between two variables.
- Linear Equation: the derivative of the highest order is a linear function of the lower order derivatives.
- First Order Linear Differential Equations: `dy/dx = p(x)y + q(x)`, where `p(x)`, `q(x)` are coefficents, which are functions of `x` alone.
- Second Order Linear Differential Equations: `d²y/dx² = p(x)dy/dx + q(x)y + r(x)`, where `p(x)`, `q(x)`, `r(x)` are coefficents, which are functions of x alone.
- Non-Linear Differential Equations: is a differential equation that is not a linear equation in the unknown function and its derivatives (from the [Wikipedia entry](https://en.wikipedia.org/wiki/Differential_equation). )

### Homogeneous: 0 is a solution of the equation.
- Homogeneous Function: in a function, if all its inputs are multiplied by a factor, then its value is multiplied by some power of this factor.
    - `f(av) = aᵏf(v)`, `f(v)` is called homogeneous of degree k.
    - `f(zx, zy) = zⁿf(x, y)`, `f(x, y)` is called homogeneous of degree n.
    - for all suitably restricted `x`, `y`, and `t`. This means that if x and y are replaced by `zx` and `zy`, `zⁿ` factors out of the resulting function, and the remaining factor is the original function.
- First Order Homogeneous differential equation: 
    - `M(x, y)dx + N(x, y)dy = 0` is homegeneous if M and N are homogeneous function of the same degree.
    - it is homogeneous, if the equation can be rewritten in the form of `y' + p(t)y = 0 or y' = -p(t)y`, where `p(t)` is any function depend on t.
- First Order Non-Homogeneous differential equation: if the equation can be rewritten in the form of `y' + p(t)y = f(t)` and `f(t)` is not zero.

### Numerical Methods
- Euler Method - First Order
    - Given `f(t, y)`
    - choose a initial value `t₀` and `y₀`
    - choose step size h and number of steps n
    - Iteration
    - ```python
        for j from 1 to n do:
            k1 = f(t, y)
            y = y + h*k1
            t = t + h
        ```

### [ODE Solution](http://www.maths.gla.ac.uk/~cc/2x/2005_2xnotes/2x_chap5.pdf)
- A solution of a differential equation is an expression for the dependent variable in terms of the independent one(s) which satisfies the relation. 
    - The general solution includes all possible solutions and typically includes arbitrary constants (in the case of an ODE) or arbitrary functions (in the case of a PDE.) 
    - A solution without arbitrary constants/functions is called a particular solution. Often we find a particular solution to a differential equation by giving extra conditions in the form of initial or boundary conditions.