module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODE, osloODE, apacheODE
) where

import Language.Drasil

import Language.Drasil.Code (FuncStmt(..), ExternalLibrary, FunctionInterface, 
  Argument, externalLib, mandatoryStep, choiceStep, libMethod, 
  libFunctionWithResult, libMethodWithResult, loopConditionMethod, 
  loopedMethod, libConstructor, lockedArg, lockedNamedArg, inlineArg, 
  inlineNamedArg, functionArg, customObjArg, recordArg, lockedParam, 
  unnamedParam, implementation, constructorInfo, methodInfo, iterateStep, 
  statementStep, lockedStatement, CodeChunk, codevar, implCQD)

import GOOL.Drasil (CodeType(Float, List, Array, Object, Func, Void))
import qualified GOOL.Drasil as C (CodeType(Boolean, Integer))

-- SciPy -- 

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ libFunctionWithResult "scipy.integrate.ode" [
    functionArg f (map unnamedParam [Float, Array Float]) 
    (\es -> FRet $ Matrix [es])] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatoryStep $ libMethod r "set_initial_value" [inlineArg Float],
  statementStep (\cdch e -> FAsg (head cdch) (Matrix [[e!!1]])),
  mandatoryStep $ loopConditionMethod r "successful" [] (\cdch -> 
    sy rt $< sy (head cdch)),
  mandatoryStep $ loopedMethod r "integrate" [inlineArg Float] (\cdch -> 
    [FAppend (sy $ head cdch) (idx (sy ry) (int 0))])] 

atol, rtol, vode :: Argument
vode = lockedArg (str "vode")
atol = inlineNamedArg "atol" Float
rtol = inlineNamedArg "rtol" Float

methodArg :: String -> Argument
methodArg = lockedNamedArg "method" . str

setIntegratorMethod :: [Argument] -> FunctionInterface
setIntegratorMethod = libMethod r "set_integrator"

odeT :: CodeType
odeT = Object "ode"

f, r, rt, ry :: CodeChunk
f = codevar $ implCQD "f_scipy" (nounPhrase "function representing ODE" 
  "functions representing ODE") Nothing (Func [Float, Float] (List Float)) 
  (Label "f") Nothing
r = codevar $ implCQD "r_scipy" (nounPhrase "ODE object" "ODE objects") Nothing 
  odeT (Label "r") Nothing
rt = codevar $ implCQD "r_t_scipy" (nounPhrase 
  "current independent variable value" "current independent variable values") 
  Nothing Float (Label "r.t") Nothing
ry = codevar $ implCQD "r_y_scipy" (nounPhrase 
  "current dependent variable value" "current dependent variable values") 
  Nothing (List Float) (Label "r.y") Nothing

-- Oslo (C#) --

osloODE :: ExternalLibrary
osloODE = externalLib [
  mandatoryStep $ libConstructor "Vector" [inlineArg Float] initv,
  choiceStep [
    libFunctionWithResult "Ode.RK547M" odeArgs sol,
    libFunctionWithResult "Ode.GearBDF" odeArgs sol],
  mandatoryStep $ libMethodWithResult sol "SolveFromToStep" 
    (map inlineArg [Float, Float, Float]) points,
  mandatoryStep $ libMethodWithResult points "ToArray" [] ptArray,
  statementStep (\cdch _ -> FAsg (head cdch) (Matrix [[]])),
  iterateStep ptArray sp 
    (\cdch -> [FAppend (sy $ head cdch) (idx (sy spX) (int 0))])]

odeArgs :: [Argument]
odeArgs = [inlineArg Float, lockedArg (sy initv),
  -- Using Matrix here is an incorrect hack to make things compile for now
  -- The type I really need is Object "Vector", but Expr does not have a way of representing a constructor call, and shouldn't since constructor calls are very code-specific and Expr should not be code-specific
  -- Probably what I need to do is define a new type, CodeExpr, which extends Expr with some code-specific representations. Then both ExternalLibrary and FuncStmt would take arguments of type CodeExpr where they currently take Expr.
  functionArg f (map unnamedParam [Float, Object "Vector"]) 
    (\es -> FRet $ Matrix [es]),
  recordArg "Options" opts ["AbsoluteTolerance", "RelativeTolerance"]]

solT :: CodeType
solT = Object "IEnumerable<SolPoint>"

initv, opts, sol, points, ptArray, sp, spX :: CodeChunk
initv = codevar $ implCQD "initv_oslo" (nounPhrase 
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables") Nothing
  (Object "Vector") (Label "initv") Nothing
opts = codevar $ implCQD "opts_oslo" (nounPhrase 
  "record containing options for ODE solving" 
  "records containing options for ODE solving") Nothing (Object "Options") 
  (Label "opts") Nothing
sol = codevar $ implCQD "sol_oslo" (nounPhrase "container for ODE information" 
  "containers for ODE information") Nothing solT (Label "sol") Nothing
points = codevar $ implCQD "points_oslo" (nounPhrase 
  "container holding ODE solution" "containers holding ODE solution") Nothing
  solT (Label "points") Nothing
ptArray = codevar $ implCQD "ptArray_oslo" (nounPhrase 
  "array holding ODE solution points" "arrays holding ODE solution points")
  Nothing (Array $ Object "SolPoint") (Label "ptArray") Nothing
sp = codevar $ implCQD "sp_oslo" (nounPhrase "ODE solution point" 
  "ODE solution points") Nothing (Object "SolPoint") (Label "sp") Nothing
spX = codevar $ implCQD "sp_X_oslo" (nounPhrase 
  "ODE solution point dependent variable" 
  "ODE solution point dependent variables") 
  Nothing (Array Float) (Label "sp.X") Nothing

-- Apache (Java) --

apacheODE :: ExternalLibrary
apacheODE = externalLib [
  choiceStep [
    libConstructor adams (lockedArg (int 3) : itArgs) it,
    libConstructor dp54 itArgs it],
  statementStep (\cdch e -> FAsg (head cdch) (Matrix [[e!!1]])),
  mandatoryStep $ libMethod it "addStepHandler" [customObjArg stepHandler 
    (implementation sh [
      methodInfo "init" (map lockedParam [t0, y0, t]) Void [statementStep 
        (\cdch _ -> FAsg (head cdch) (Matrix [[idx (sy y0) (int 0)]]))],
      methodInfo "handleStep" (map lockedParam [interpolator, isLast]) Void [
        mandatoryStep $ libMethodWithResult interpolator "getInterpolatedState" 
          [] curr,
        statementStep (\cdch _ -> 
          FAppend (sy $ head cdch) (idx (sy curr) (int 0)))]])],
  mandatoryStep $ libMethod it "integrate" (customObjArg ode
    (implementation "FirstOrderDifferentialEquations" [
      constructorInfo [],
      methodInfo "getDimension" [] C.Integer [lockedStatement $ FRet (int 1)],
      methodInfo "computeDerivatives" [
        lockedParam t, unnamedParam (Array Float), unnamedParam (Array Float)]
        Void [statementStep (\cdch e -> FAsgIndex (head cdch) 0 (head e))]]) : 
    map inlineArg [Float, Array Float, Float, Array Float])]

itArgs :: [Argument]
itArgs = map inlineArg [Float, Float, Float, Float]

adams, dp54, foi, sh :: String
adams = "AdamsBashforthIntegrator"
dp54 = "DormandPrince54Integrator"
foi = "FirstOrderIntegrator"
sh = "StepHandler"

it :: CodeChunk
it = codevar $ implCQD "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") Nothing (Object foi) (Label "it") Nothing

stepHandler, t0, y0, t, interpolator, isLast, curr, ode :: CodeChunk
stepHandler = codevar $ implCQD "stepHandler_apache" (nounPhrase 
  "ODE step handler" "ODE step handlers") Nothing (Object sh)
  (Label "stepHandler") Nothing
t0 = codevar $ implCQD "t0_apache" (nounPhrase "initial time for ODE solving"
  "intial times for ODE solving") Nothing Float (Label "t0") Nothing
y0 = codevar $ implCQD "y0_apache" (nounPhrase 
  "array of initial values for ODE solving" 
  "arrays of initial values for ODE solving") 
  Nothing (Array Float) (Label "y0") Nothing
t = codevar $ implCQD "t_apache" (nounPhrase "current time in ODE solution"
  "current times in ODE solution") Nothing Float (Label "t") Nothing
interpolator = codevar $ implCQD "interpolator_apache" (nounPhrase 
  "step interpolator for ODE solving" "step interpolator for ODE solving")
  Nothing (Object "StepInterpolator") (Label "interpolator") Nothing
isLast = codevar $ implCQD "isLast_apache" (nounPhrase 
  "boolean for whether the current step is the last step"
  "booleans for whether the current step is the last step")
  Nothing C.Boolean (Label "isLast") Nothing
curr = codevar $ implCQD "curr_apache" (nounPhrase 
  "ODE solution array for current step" "ODE solution arrays for current step")
  Nothing (Array Float) (Label "curr") Nothing
ode = codevar $ implCQD "ode_apache" (nounPhrase 
  "object representing an ODE system" "objects representing an ODE system")
  Nothing (Object "ODE") (Label "ode") Nothing