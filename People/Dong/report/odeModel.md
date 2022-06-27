# Background
what can do before
- solve first order ode by writing down explicit equation, lhs = rhs

what need to change
- Capture knowledge of ODE, and generate the explicit equation

what can do now
- automatically solve a high-order linear ODE

# Content

## Internal data represent, AX = c, 
In general, a equation contains a left hand expression, a right hand expression, and a equal sign. The left hand expression and the right hand expression connect with each other by equal sign. We can write a linger ODE in a shape of **Ax** = **b** (Scientific computing citation p48).On the left hand side, A is a known m * n matrix and b is an m-vector. On the right hand side x is an n-vector.

The A is commonly know as coefficient matrix, b is the constant vector, and x is the unknown vector.

```
yₜ'' + (1 + Kd)yₜ' + (20 + Kₚ)yₜ = rₜKₚ
```
To take an example, in above equation, yₜ is the dependent variable, and Kd, Kₚ rₜ, Kₚ are constant variables. We can write this equation in a form of **Ax** = **b**

A = 1 (1 + Kd) (20 + Kₚ)
x =  yₜ''
     yₜ'
     yₜ
b = rₜKₚ

Base on this analysis, we decide to create a datatype called `DifferentialModel` to capture the ODE knowledge. The `DifferentialModel` consists of the independent variable, the dependent variable, the coefficient matrix, the unknown vector, the constant vector, and its meta data.

```haskell
data DifferentialModel = SystemOfLinearODEs {
  _indepVar :: UnitalChunk,
  _depVar :: ConstrConcept,
  _coefficients :: [[Expr]],
  _unknowns :: [Unknown],
  _dmConstants :: [Expr],
  _dmconc :: ConceptChunk
}
```
- _indepVar represent the independent variable in ODE, and it is usually time. UnitalChunk is a Quantity(concept with a symbolic representation) with a Unit !!cite Dan paper!!
- _depVar represent the dependent variable in ODE. ConstrConcept is !!cite Dan paper!!
- _coefficients represent the coefficient of the ODE, it is A in **Ax** = **b**. A list of lists represent a matrix. `Expr` is a type encode mathematical expression. !!Cite Brooks!!
- _unknowns represent the unknown vector, it is x in **Ax** = **b**. The `Unknown` is synonym of Integer, it indicates nth order of derivative of the dependent variable
- _dmConstants represent the constant vector, it is b in **Ax** = **b**. It is a list of `Expr`
- _dmconc: ConceptChunk records a concept !!citation in here!!

### future improvement
Currently, the `DifferentialModel` only capture knowledge of ODEs with one dependent variable. This is a special case of the family of linear ODEs. Studying this special case will help the Drasil team better understand how to capture the knowledge of the ODEs, and eventually lead to solve a system of linear ODE with multiple dependent variables.

### Input language 
We introduced an input language to simplify the input a single ODE, because it could be over complicated for users to input a single ODE in to a matrix form. The structure of the input language will mimic the mathematical expression of linear differential equation that was bases on https://tutorial.math.lamar.edu/Classes/DE/Definitions.aspx,

aₙ(t)yⁿ(t) + aₙ₋₁(t)yⁿ⁻¹(t) +... + a₁(t)y′(t) + a₀(t)y(t) = g(t)

The left hand side of equations is a collection of terms. A term consist a coefficient and a derivative of the dependent variable.

The following is the detail of code for new type introduced
```haskell 
type Unknown = Integer

data Term = T{
  _coeff :: Expr,
  _unk :: Unknown
}

type LHS = [Term]
```
- The `LHS` reflects to left hand side, and consist of a sequence of `Term`.
- A `Term` contains a `Unknown`, and a `Expr`
- A `Unknown` is a integer, indicate nth derivative of the dependent variable

The following are new operators introduced
```haskell
($^^) :: ConstrConcept -> Integer -> Unknown
($^^) _ unk' = unk'

($*) :: Expr -> Unknown -> Term
($*) = T

($+) :: [Term] -> Term -> LHS
($+) xs x  = xs ++ [x]
```
- `$^^` take a `ConstrConcept` and `Integer` to form a `Unknown`. The `ConstrConcept` is the dependent variable and `Integer` is the order of nth derivative. Eg, depVar $^^ d, means nth derivative of depVar.
- `$*` take a `Expr` and `Unknown` to form a `Term`. The `Expr` is the coefficient and `Unknown` is the nth derivative
- `$+` take a `[Term]` and `Term` to form a new `[Term]` by appending `Term` to `[Term]`

Here is a comprehensive example. To encode the following equation

```
yₜ'' + (1 + Kd)yₜ' + (20 + Kₚ)yₜ = rₜKₚ
```

with equivalent variable names in PDController case study
- qdDerivGain is Kd
- opProcessVariable is yₜ
- qdPropGain is Kₚ
- qdSetPointTD is rₜ
we can write them as the following
```haskell
lhs = [exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1)] --line 1
      $+ (exactDbl 1 $* (opProcessVariable $^^ 2)) -- line 2
      $+ (exactDbl 20 `addRe` sy qdPropGain $* (opProcessVariable $^^ 0))
rhs = sy qdSetPointTD `mulRe` sy qdPropGain
```
lhs represents the left hand side and rhs represents the right hand side. lhs is a type of LHS, and LHS is synonym for `[Term]`. rhs is just a `Expr`.

- In line 1, the whole syntax create a single list of `Term`. In side of this single `Term`, everything before the `$*` is the coefficient, and everything after the `$*` is the `Unknown`. The coefficient is `1 + Kd`, and the `Unknown` is the first derivative of yₜ.
- In line 2, everything after the `$+` is a `Term`. By using `$+`, we append a `Term` into a list of `Term`.

The order of Term is not matter in this case, the user can put any order, they all eventually come out the same result.

### Two constructors
In the implementation, there are two constructors that create a `DifferentialModel`.

The first constructor is `makeASystemDE`. A user can set the coefficient matrix, unknown vector, and constant vector by explicitly giving `[[Expr]]`, `[Unknown]`, and `[Expr]`. There will be several guards to check wether inputs is well-formed.
1. matrix and constant vector: `_coefficients` is a m * n matrix and `_dmConstants` is a m vector. This guard make sure they have the same m dimension. If there is error says "Length of coefficients matrix should equal to the length of the constant vector", it means `_coefficients` and `_dmConstants` has different m dimension, and it violates mathematical rules.

2. each row in matrix and unknown vector: `_coefficients` use list of list to represent a m * n matrix. It means each list in `_coefficients` will have the same length n, and `_unknowns` is n vector. Therefore, the length of each row in the `_coefficients` should be equal to the length of `_unknowns`. The following code will create a math expression to indicate wether inputs for `_coefficients` and `_unknowns` are valid.

```haskell
isCoeffsMatchUnknowns :: [[Expr]] -> [Unknown] -> Bool
isCoeffsMatchUnknowns [] _ = error "Coefficients matrix can not be empty"
isCoeffsMatchUnknowns _ [] = error "Unknowns column vector can not be empty"
isCoeffsMatchUnknowns coeffs unks = foldr (\ x -> (&&) (length x == length unks)) True coeffs
```

If there is error says "The length of each row vector in coefficients need to equal to the length of unknowns vector", it means `_coefficients` and `_unknowns` violate mathematical rules.

3. the order of unknown vector: this guard is designed for simplifying implementation. We have no control on what users give to us. There are infinite ways to represent a linear equation in the form of **Ax** = **b**, so we strictly ask users put the order of the unknown vector descending. This design choice will help to simply the later transformation from **Ax** = **b** to **X**' = **AX** + **c** (link here). If there is error says "The order of giving unknowns need to be descending", it means the order of unknown vector is not descending

The following is the example how to directly set the coefficient matrix, unknown vector, and constant vector
```haskell
eBalanceOnWtrRC :: DifferentialModel 
eBalanceOnWtrRC = 
  makeASystemDE
    time
    tempW
    coeffs
    unknowns
    constants
    "eBalanceOnWtrRC" 
    (nounPhraseSP $ "Energy balance on " ++ "water to find the temperature of the water") 
    (tempW ^. defn)
    where coeffs = [[exactDbl 1, recip_ (sy tauW)]]
          unknowns = [1, 0]
          constants = [recip_ (sy tauW) `mulRe` sy tempC]
```
In `makeASystemDE`, it requires users to input the unknown descending.


The second constructor is called `makeASingleDE`. This constructor uses the input language to simplify the input of a single ODE. The following is the example how to use input language to create a `DifferentialModel`.
```haskell
imPDRC :: DifferentialModel
imPDRC
  = makeASingleDE
      time
      opProcessVariable
      lhs
      rhs
      "imPDRC"
      (nounPhraseSP "Computation of the Process Variable as a function of time")
      EmptyS
      where lhs = [exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1)]
                  $+ (exactDbl 1 $* (opProcessVariable $^^ 2))
                  $+ (exactDbl 20 `addRe` sy qdPropGain $* (opProcessVariable $^^ 0))
            rhs = sy qdSetPointTD `mulRe` sy qdPropGain
```
In `makeASingleDE`, users don't necessary to input the unknown vector in descending order. Any order will be fine, because we will sort out the unknown vector base on its derivative and build the coefficient matrix accordingly. Once users enter the ODE equations by using the input language, there are several steps to generate its data represent.
- First, we create a descending unknown vector base on the highest order of the derivatives. For example, the highest order in the lhs is 2, then we will generate 2, 1 and 0 derivative respectively.
```haskell
unks = createAllUnknowns(findHighestOrder lhs ^. unk) depVar''

findHighestOrder :: LHS -> Term
findHighestOrder = foldr1 (\x y -> if x ^. unk >= y ^. unk then x else y)

createAllUnknowns :: Unknown -> ConstrConcept -> [Unknown]
createAllUnknowns highestUnk depv
  | highestUnk  == 0  = [highestUnk]
  | otherwise = highestUnk : createAllUnknowns (highestUnk - 1) depv
```
- Second, we will create the coefficient matrix base on the descending unknown vector. For each derivative, we search it coefficient base on the order of derivative. 
```haskell
coeffs = [createCoefficients lhs unks]

createCoefficients :: LHS -> [Unknown] -> [Expr]
createCoefficients [] _ = error "Left hand side is an empty list"
createCoefficients _ [] = []
createCoefficients lhs (x:xs) = genCoefficient (findCoefficient x lhs) : createCoefficients lhs xs
```
It would be tedious to require users to input a matrix form for a single ODE. Therefore, we create an input language to make it easier to input such equations.

### display matrix , linear equation or normal form
After users enter ODE equations, we display them in SRS. There are two different ways to display equations, matrix and linear equations.
- display matrix
  - when there are multiple ODE equations, we display ODE in **Ax** = **b**
- linear equation
  - when there is only a single ODE equations, we display ODE in
  - aₙ(t)y⁽ⁿ⁾(t) + aₙ₋₁(t)yⁿ⁻¹(t)+ ... + a₁(t)y′(t) + a₀(t)y(t) = g(t) https://sites.oxy.edu/ron/math/341/10/ws/01.pdf p2
  https://tutorial.math.lamar.edu/classes/de/HOBasicConcepts.aspx
  - consistent with the input language

- normal form (future option)
  - in further, the Drasil team will investigate more variability of displaying ODE.
  - y⁽ⁿ⁾ = f(x, y , y', ..., yⁿ⁻¹) https://sites.oxy.edu/ron/math/341/10/ws/01.pdf
