# Background
- In the chapter 1, we discover how to represent the ODE in a `DifferentialModel`. In the chapter 2, we discover different external ODE libraries. In this chapter, we will discover how to connect the `DifferentialModel` with external ODE libraries.

In the chapter 2, we know all selected external libraries are eligible to solve a system of first order ODE. If we can convert a high-order linear  ODE to a system of first order ODE and generate proper interface to connect with those external libraries, then we can solve the high order ODE numerically.

In the Paul's Online Notes (https://tutorial.math.lamar.edu/classes/de/systemsde.aspx), example 2, it highlighted all the steps how to rewrite it into a system. Although the example only show how to rewrite a 4th order linear ode into a system, it is not hard to produce a generic instruction for solving nth order linear ODE. 

y'''' + 3y'' − sin(t) * y' + 8y = t²
- as the example2 show in the Paul's Online Notes, 4th order ODE will eventually be written to 4th first order ODE with four new variables be introduced. 

- if we have a nth order linear order, we can transform it to a a system of ODE with n equations.
- We introduce n variables from x1, x2, ..., xn

y⁽ⁿ⁾ + 3y'' − sin(t) * y' + 8y = t²

x₁ = y      ⇒ x₁' = y'  = x₂ 
x₂ = y'     ⇒ x₂' = y'' = x₃
x₃ = y''    ⇒ x₃' = y'''= x₄
...
xₙ = y⁽ⁿ⁻¹⁾ ⇒ xₙ' = y⁽ⁿ⁾= −8y + sin(t)y' − 3y'' + t² = −8x₁ + sin(t)x₂ − 3x₃ + t²

This is the road map how the Drasil framework will solve the ODE via external libraries.

## transform matrix represent to solver format (X' = AX + c)
According to "http://www.ohiouniversityfaculty.com/youngt/IntNumMeth/lecture29.pdf", most high order ODEs can transform into an equivalent system of first-order equation.
The following is the more generic way to transform a high-order ode to a system in shape of X' = AX + c. 

https://github.com/JacquesCarette/Drasil/issues/3016

1. isolate the hightest derivative
  x⁽ⁿ⁾ = f (t, x, x', x'', ..., x⁽ⁿ⁻¹⁾)
2. Introduce new variables 
  y₁ = x
  y₂ = x'
  ...
  yₙ = x⁽ⁿ⁻¹⁾
3. differentiate the new variables
  y₁' = x' = y₂
  y₂' = x'' = y₃
  ...
  yₙ' = x⁽ⁿ⁾ = f (t, y₁, y₂, ..., yₙ)

4. rewrite the linear function 
  - the f (t, y₁, y₂, ..., yₙ) is a linear function, we can rewrite is as  (wiki)
```
h(t) + g1(t)*y1 + g2(t)*y2 + ... + gn(t)*yn
```
5. summary them in a matrix form X' = AX + c
```
[y1'] = [0      1       0     ...    0 ] [y1 ] = [0]
[y2'] = [0      0       1     ...    0 ] [y2 ]   [0]
[...] = [ ...                          ] [...]   [0]
[yn'] = [g1(t)  g2(t)   g3(t) ... gn(t)] [yn ]   [h(t)]
```

### Implementation
Connect `DifferentialModel` via `ODEInfo`

background: `ODEInfo` serve as a interface to connect ODE knowledge with external libraries. Users manually extract ODE knowledge from SRS, and store in the `ODEInfo`. It gathers necessary information for external libraries to solve the ODE equation numerically.

Manually creating `ODEInfo` against the principle of the Drasil framework. The reason this need to done manually because we haven't establish a scientific and systematic way to capture ODE knowledge. Another huge disadvantage of manually creating data is that it cause duplication.

With the ODE information be captured by `DifferentialModel`, we can automatically create `ODEInfo`

```haskell
odeInfo' :: [CodeVarChunk] -> ODEOptions -> DifferentialModel -> InitialValueProblem -> ODEInfo
odeInfo' ovs opt dm ivp = ODEInfo 
  (quantvar $ _indepVar dm) 
  (quantvar $ _depVar dm) 
  ovs 
  (expr $ initTime ivp)
  (expr $ finalTime ivp)
  (map expr $ initValues ivp)
  (createFinalExpr dm)
  opt

createFinalExpr :: DifferentialModel -> [CodeExpr]
createFinalExpr dm = map expr $ formEquations (coeffVects ode) (unknownVect ode) (constantVect ode) (_depVar dm)
  where ode = makeAODESolverFormat dm

formEquations :: [[Expr]] -> [Unknown] -> [Expr] -> ConstrConcept-> [Expr]
formEquations [] _ _ _ = []
formEquations _ [] _ _ = []
formEquations _ _ [] _ = []
formEquations (ex:exs) unks (y:ys) depVa =
  (if y == exactDbl 0 then finalExpr else finalExpr `addRe` y) : formEquations exs unks ys depVa
  where indexUnks = map (idx (sy depVa) . int) unks -- create X
        filteredExprs = filter (\x -> fst x /= exactDbl 0) (zip ex indexUnks) -- remove zero coefficients
        termExprs = map (uncurry mulRe) filteredExprs -- multiple coefficient with depend variables
        finalExpr = foldl1 addRe termExprs -- add terms together
```

In fact, all the data used have been storing in the `DifferentialModel`. Giving the generic instruction, we can automate the transform.

the `DifferentialModel` represent ODE in form of **AX** = **b**, this can be transfer to **X**' = **AX** + **c** in the following code. One thing need to keep note is that the A and X in those two equations are different.

ODESolverFormat data type is created to represent X' = AX + c
```haskell
data ODESolverFormat = X'{
  coeffVects :: [[Expr]],
  unknownVect :: [Integer],
  constantVect :: [Expr]
}
```

- `transUnks` return  X
- `transEs` return A
- `transConsts` return c
```haskell
makeAODESolverFormat :: DifferentialModel -> ODESolverFormat
makeAODESolverFormat dm = X' transEs transUnks transConsts
  where transUnks = transUnknowns $ dm ^. unknowns
        transEs = addIdentityCoeffs [transCoefficients $ head (dm ^. coefficients)] (length transUnks) 0
        transConsts = addIdentityConsts [head (dm ^. dmConstants) `divideCosntants` head (head (dm ^. coefficients))] (length transUnks)
```

### transUnks
Since we strictly require unknown vector to be descending, the first element is the highest directive. `transUnks` simply trim off the highest directive.

```haskell
transUnknowns :: [Unknown] -> [Unknown]
transUnknowns = tail
```

### transEs
To create the matrix A, 

1. we have to make sure the coefficient of the highest order is 1. if it is not one: we reduce the coefficient by divide the coefficient itself for every element in the relation
2. then, remove the highest coefficient.

```haskell
transCoefficients :: [Expr] -> [Expr]
transCoefficients es
  | head es == exactDbl 1 = mapNeg $ tail es
  | otherwise = mapNeg $ tail $ map ($/ head es) es
    where mapNeg = map (\x -> if x == exactDbl 0 then exactDbl 0 else neg x)
```

3. create identity matrix and combine with the giving matrix. addIdentityCoeffs take the new matrix from `transCoefficients`, add identity matrix above it.
```haskell
addIdentityCoeffs :: [[Expr]] -> Int -> Int -> [[Expr]]
addIdentityCoeffs es len index
  | len == index + 1 = es
  | otherwise = addIdentityCoeffs (constIdentityRowVect len index : es) len (index + 1)

constIdentityRowVect :: Int -> Int -> [Expr]
constIdentityRowVect len index = addIdentityValue index $ replicate len $ exactDbl 0

addIdentityValue :: Int -> [Expr] -> [Expr]
addIdentityValue n es = fst splits ++ [exactDbl 1] ++ tail (snd splits)
  where splits = splitAt n es
```

4. create the constant matrix. We have to make sure whether the coefficient of the hightest order in original equation is 1 or not. If is not 1, we have to divide the constant vector before add zero
### transConsts
```haskell
addIdentityConsts :: [Expr] -> Int -> [Expr]
addIdentityConsts expr len = replicate (len - 1) (exactDbl 0) ++ expr

divideCosntants :: Expr -> Expr -> Expr
divideCosntants a b
  | b == exactDbl 0 = error "Divisor can't be zero"
  | b == exactDbl 1 = a
  | otherwise       = a $/ b
```

Once we create`ODESolverFormat`, there are three more data type we need create

5. InitialValueProblem: contain information need to solve IVP. 
  - initTime: initial time
  - finalTime: final time (can be a time interval https://github.com/JacquesCarette/Drasil/issues/2194#issuecomment-1098040003)
  - initValues: initial values for newly created first order equations

```haskell
data InitialValueProblem = IVP{
  initTime :: Expr,
  finalTime :: Expr,
  initValues :: [Expr]
}
```

6. ODEOptions
`ODEOptions` !cite brook thesis!

7. manually provide other variables
otherVars !cite brook thesis!

explain why this part still need to do manually, it possible the user give a `Expr` that has other variable in this `Expr`. the Drasil framework is not ready to extract symbols from a `Expr`. Further research on symbol manipulation could enhance the automation in this part.

provide initial values (y₁, y₂, ..., yₙ) for the system of first-order ODE in input.txt file

## the gap between SRS and ODE solvers
the way handle the dependent variable. the problem arise when there are multiple dependent variables
https://github.com/JacquesCarette/Drasil/issues/2986

In order to solve a high-order ODEs numerically, we need to transform it to a system of first-order equations. However, SRS is not responsible tell user how to solve the ODE. We believe we need a something in between. In future, the drasil team want to generate a design document to bridge the gap between SRS and external libraries.

# upgraded interface for solving high order ODE
- change the initial value to a list
    - initialize array with element(too minor)
- change in Apache Java interface !!cite here!!
```java
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations;

public class ODE implements FirstOrderDifferentialEquations {
  private double K_p;
  private double K_d;
  private double r_t;
  
  public ODE(double K_p, double K_d, double r_t) {
    this.K_p = K_p;
    this.K_d = K_d;
    this.r_t = r_t;
  }
  
  public int getDimension() {
    return 2;
  }

  public void computeDerivatives(double t, double[] y_t, double[] dy_t) {
    dy_t[0] = y_t[1];
    dy_t[1] = -(1.0 + K_d) * y_t[1] + -(20.0 + K_p) * y_t[0] + r_t * K_p;
  }
}
```

# manually build solve format for double pendulum (non-linear ODE example)
Keep manually building `ODEInfo`

the double pendulum is an example of non-linear ODE. The `DifferentialModel` represent ODE in form of **AX** = **b**. For a non-linear ODE, we can not easily write it in form of **AX** = **b**. In such case, 

possible cite here

```haskell
dblPenODEInfo :: ODEInfo
dblPenODEInfo = odeInfo
  (quantvar time)
  (quantvar pendDisAngle)
  [quantvar massObj_1, quantvar massObj_2, quantvar lenRod_1, quantvar lenRod_2]
  (exactDbl 0)
  (exactDbl 20) -- final time
  [dbl 1.3463968515384828, exactDbl 0, dbl 2.356194490192345, exactDbl 0] -- unit in radian [3*pi/7, 0, 3*pi/4, 0]
  [ o1,
    neg g `mulRe`
      (two `mulRe` m1 `addRe` m2) `mulRe` sin t1 $-
      (m2 `mulRe` g `mulRe` sin (t1 $- (two `mulRe` t2))) $-
      ((two `mulRe` sin (t1 $- t2 )) `mulRe` m2 `mulRe`
      (square o2 `mulRe` l2 `addRe`
      (square o1 `mulRe` l1 `mulRe` cos (t1 $- t2))))
      $/
      l1 `mulRe`(two `mulRe` m1 `addRe` m2 $-
      (m2 `mulRe` cos (two `mulRe` t1  $- (two `mulRe` t2)))),
    o2,
    two `mulRe` sin (t1 $- t2) `mulRe`
      (square o1 `mulRe` l1 `mulRe` (m1 `addRe` m2 ) `addRe`
      (g `mulRe` (m1 `addRe` m2 ) `mulRe` cos t1) `addRe`
      (square o2 `mulRe` l2 `mulRe` m2 `mulRe` cos (t1 $- t2 )))
      $/
      l2 `mulRe`(two `mulRe` m1 `addRe` m2 $-
      (m2 `mulRe` cos (two `mulRe` t1  $- (two `mulRe` t2))))
    ]
  dblPenODEOpts
    where t1  = idx (sy pendDisAngle) (int 0) -- t1 is theta 1
          o1  = idx (sy pendDisAngle) (int 1) -- o1 is omega 1
          t2  = idx (sy pendDisAngle) (int 2) -- t2 is theta 2
          o2  = idx (sy pendDisAngle) (int 3) -- o2 is omega 2
          g   = dbl 9.8 -- should be sy gravitationalAccelConst but there is a bug
                        -- https://github.com/JacquesCarette/Drasil/issues/2998
          m1  = sy massObj_1
          m2  = sy massObj_2
          two = exactDbl 2
          l1  = sy lenRod_1
          l2  = sy lenRod_2
```

Instance model screen shots
