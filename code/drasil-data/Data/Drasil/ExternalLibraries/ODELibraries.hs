module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODE, oslo, apacheODE, odeint
) where

import Language.Drasil

import Language.Drasil.Code (ExternalLibrary, Step, Argument, 
  externalLib, mandatoryStep, mandatorySteps, choiceSteps, choiceStep, callStep,
  callWithImport, callWithImports, libFunction, libMethod, 
  libFunctionWithResult, libMethodWithResult, libConstructor, 
  constructAndReturn, lockedArg, lockedNamedArg, inlineArg, inlineNamedArg, 
  preDefinedArg, functionArg, customObjArg, recordArg, lockedParam, 
  unnamedParam, customClass, implementation, constructorInfo, methodInfo, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn, CodeChunk, codevar, codefunc, ccObjVar, implCQD)

import GOOL.Drasil (CodeType(Float, List, Array, Object, Void))
import qualified GOOL.Drasil as C (CodeType(Boolean, Integer))

-- SciPy -- 

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ callWithImport scipyImport $ libFunctionWithResult 
    odefunc [
      functionArg f (map unnamedParam [Float, Array Float]) 
      returnExprList] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatorySteps [callStep $ libMethod r setInitVal [inlineArg Float],
    initSolListWithVal,
    solveAndPopulateWhile (libMethod r successful []) rt 
      (libMethod r integrateStep [inlineArg Float]) ry]]

scipyImport :: String
scipyImport = "scipy.integrate"

atol, rtol, vode :: Argument
vode = lockedArg (str "vode")
atol = inlineNamedArg "atol" Float
rtol = inlineNamedArg "rtol" Float

methodArg :: String -> Argument
methodArg = lockedNamedArg "method" . str

setIntegratorMethod :: [Argument] -> Step
setIntegratorMethod = callStep . libMethod r setIntegrator

odeT :: CodeType
odeT = Object "ode"

f, r, rt, ry, odefunc, setIntegrator, setInitVal, successful, 
  integrateStep :: CodeChunk
f = codevar $ implCQD "f_scipy" (nounPhrase "function representing ODE system" 
  "functions representing ODE system") Nothing (List Float) (Label "f") Nothing
r = codevar $ implCQD "r_scipy" (nounPhrase "ODE object" "ODE objects") Nothing 
  odeT (Label "r") Nothing
rt = ccObjVar r t
ry = ccObjVar r y
odefunc = codefunc $ implCQD "ode_scipy" (nounPhrase 
  "function for defining an ODE for SciPy" 
  "functions for defining an ODE for SciPy") Nothing 
  odeT (Label (scipyImport ++ ".ode")) Nothing
setIntegrator = codefunc $ implCQD "set_integrator_scipy" (nounPhrase
  "method for setting SciPy integrator" "methods for setting SciPy integrator")
  Nothing Void (Label "set_integrator") Nothing
setInitVal = codefunc $ implCQD "set_initial_value_scipy" (nounPhrase
  "method for setting initial value for ODE for SciPy" 
  "methods for setting initial value for ODE for SciPy")
  Nothing Void (Label "set_initial_value") Nothing
successful = codefunc $ implCQD "successful_scipy" (nounPhrase 
  "method returning True if integration is current successful"
  "methods returning True if integration is current successful")
  Nothing C.Boolean (Label "successful") Nothing
integrateStep = codefunc $ implCQD "integrate_scipy" (nounPhrase
  "method that performs one integration step on an ODE"
  "methods that perform one integration step on an ODE")
  Nothing Void (Label "integrate") Nothing

-- Oslo (C#) --

oslo :: ExternalLibrary
oslo = externalLib [
  mandatoryStep $ callWithImport "Microsoft.Research.Oslo" $ libConstructor 
    vector [inlineArg Float] initv,
  choiceStep $ map (\s -> callStep $ libFunctionWithResult s odeArgs sol) 
    [rk547m, gearBDF],
  mandatorySteps (callWithImport "System.Linq" (libMethodWithResult sol 
      solveFromToStep (map inlineArg [Float, Float, Float]) points) :
    populateSolList points sp x)]

odeArgs :: [Argument]
odeArgs = [inlineArg Float, lockedArg (sy initv),
  functionArg fOslo (map unnamedParam [Float, vecT]) 
    (callStep $ constructAndReturn vector [inlineArg Float]),
  recordArg "Options" opts ["AbsoluteTolerance", "RelativeTolerance"]]

solT, vecT :: CodeType
solT = Object "IEnumerable<SolPoint>"
vecT = Object "Vector"

initv, fOslo, opts, sol, points, sp, x, vector, rk547m, gearBDF, 
  solveFromToStep :: CodeChunk
initv = codevar $ implCQD "initv_oslo" (nounPhrase 
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables") Nothing
  vecT (Label "initv") Nothing
fOslo = codevar $ implCQD "f_oslo" (nounPhrase 
  "function representing ODE system" "functions representing ODE system") 
  Nothing vecT (Label "f") Nothing
opts = codevar $ implCQD "opts_oslo" (nounPhrase 
  "record containing options for ODE solving" 
  "records containing options for ODE solving") Nothing (Object "Options") 
  (Label "opts") Nothing
sol = codevar $ implCQD "sol_oslo" (nounPhrase "container for ODE information" 
  "containers for ODE information") Nothing solT (Label "sol") Nothing
points = codevar $ implCQD "points_oslo" (nounPhrase 
  "container holding ODE solution" "containers holding ODE solution") Nothing
  solT (Label "points") Nothing
sp = codevar $ implCQD "sp_oslo" (nounPhrase "ODE solution point" 
  "ODE solution points") Nothing (Object "SolPoint") (Label "sp") Nothing
x = codevar $ implCQD "X_oslo" (nounPhrase "dependent variable" 
  "dependent variables") Nothing (Array Float) (Label "X") Nothing
vector = codefunc $ implCQD "Vector_oslo" (nounPhrase 
  "constructor for an OSLO Vector" "constructors for an OSLO Vector")
  Nothing vecT (Label "Vector") Nothing
rk547m = codefunc $ implCQD "RK547M_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Runge-Kutta method"
  "functions for initiating an ODE to be solved by Runge-Kutta method")
  Nothing solT (Label "Ode.RK547M") Nothing
gearBDF = codefunc $ implCQD "GearBDF_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Gear's BDF method"
  "functions for initiating an ODE to be solved by Gear's BDF method")
  Nothing solT (Label "Ode.GearBDF") Nothing
solveFromToStep = codefunc $ implCQD "SolveFromToStep_oslo" (nounPhrase
  "method for solving an ODE given a time range" 
  "methods for solving an ODE given a time range")
  Nothing solT (Label "SolveFromToStep") Nothing

-- Apache (Java) --

apacheODE :: ExternalLibrary
apacheODE = externalLib [
  choiceStep [
    callWithImports [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ adams]
      $ libConstructor adamsC (lockedArg (int 3) : itArgs) it,
    callWithImports [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ dp54]
      $ libConstructor dp54C itArgs it],
  mandatorySteps [callStep $ libMethod it addStepHandler [
      customObjArg (map ((apacheImport ++ "sampling.") ++) [sh, si]) 
        stepHandler (implementation sh [
          methodInfo "init" (map lockedParam [t0, y0, t]) Void 
            [initSolListFromArray y0],
          methodInfo "handleStep" (map lockedParam [interpolator, isLast]) Void 
            [callStep $ libMethodWithResult interpolator getInterpState [] curr,
            appendCurrSol curr]])],
    callStep $ libMethod it integrate (customObjArg 
      [apacheImport ++ fode] ode (implementation fode [
        constructorInfo [] [],
        methodInfo "getDimension" [] C.Integer [fixedReturn (int 1)],
        methodInfo "computeDerivatives" [
          lockedParam t, unnamedParam (Array Float), unnamedParam (Array Float)]
          Void [assignArrayIndex 0]]) : 
      [inlineArg Float, preDefinedArg currVals, inlineArg Float, 
        preDefinedArg currVals]),
    assignSolFromObj stepHandler]]

apacheImport :: String
apacheImport = "org.apache.commons.math3.ode."

itArgs :: [Argument]
itArgs = map inlineArg [Float, Float, Float, Float]

adams, dp54, foi, sh, si, fode :: String
adams = "AdamsBashforthIntegrator"
dp54 = "DormandPrince54Integrator"
foi = "FirstOrderIntegrator"
sh = "StepHandler"
si = "StepInterpolator"
fode = "FirstOrderDifferentialEquations"

it, currVals, stepHandler, t0, y0, interpolator, isLast, curr, adamsC, 
  dp54C, addStepHandler, getInterpState, integrate :: CodeChunk
it = codevar $ implCQD "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") Nothing (Object foi) (Label "it") Nothing
currVals = codevar $ implCQD "curr_vals_apache" (nounPhrase 
  "array holding ODE solution values for the current step"
  "arrays holding ODE solution values for the current step") Nothing 
  (Array Float) (Label "curr_vals") Nothing
stepHandler = codevar $ implCQD "stepHandler_apache" (nounPhrase 
  "ODE step handler" "ODE step handlers") Nothing (Object sh)
  (Label "stepHandler") Nothing
t0 = codevar $ implCQD "t0_apache" (nounPhrase "initial time for ODE solving"
  "intial times for ODE solving") Nothing Float (Label "t0") Nothing
y0 = codevar $ implCQD "y0_apache" (nounPhrase 
  "array of initial values for ODE solving" 
  "arrays of initial values for ODE solving") 
  Nothing (Array Float) (Label "y0") Nothing
interpolator = codevar $ implCQD "interpolator_apache" (nounPhrase 
  "step interpolator for ODE solving" "step interpolator for ODE solving")
  Nothing (Object si) (Label "interpolator") Nothing
isLast = codevar $ implCQD "isLast_apache" (nounPhrase 
  "boolean for whether the current step is the last step"
  "booleans for whether the current step is the last step")
  Nothing C.Boolean (Label "isLast") Nothing
curr = codevar $ implCQD "curr_apache" (nounPhrase 
  "ODE solution array for current step" "ODE solution arrays for current step")
  Nothing (Array Float) (Label "curr") Nothing
adamsC = codefunc $ implCQD "adams_ctor_apache" (nounPhrase
  "constructor for an Adams-Bashforth integrator" 
  "constructors for an Adams-Bashforth integrator") 
  Nothing (Object adams) (Label adams) Nothing
dp54C = codefunc $ implCQD "dp54_ctor_apache" (nounPhrase
  "constructor for a Dormand-Prince 5-4 integrator"
  "constructors for a Dormand-Prince 5-4 integrator")
  Nothing (Object dp54) (Label dp54) Nothing
addStepHandler = codefunc $ implCQD "addStepHandler_apache" (nounPhrase
  "method for adding a step handler to an integrator"
  "methods for adding a step handler to an integrator")
  Nothing Void (Label "addStepHandler") Nothing
getInterpState = codefunc $ implCQD "getInterpolatedState_apache" (nounPhrase
  "method for getting current state during ODE solving"
  "methods for getting current state during ODE solving")
  Nothing (Array Float) (Label "getInterpolatedState") Nothing
integrate = codefunc $ implCQD "integrate_apache" (nounPhrase
  "method for integrating an ODE" "methods for integrating an ODE")
  Nothing Void (Label "integrate") Nothing

-- odeint (C++) --

odeint :: ExternalLibrary
odeint = externalLib [
  choiceSteps [
    [callStep $ libConstructor rkdp5C [] rk,
    callStep $ libFunctionWithResult makeControlled 
      [inlineArg Float, inlineArg Float, lockedArg (sy rk)] stepper],
    [callStep $ libConstructor adamsBashC [] stepper]],
  mandatoryStep $ callWithImport "boost/numeric/odeint.hpp" $ libFunction 
    integrateConst [
      lockedArg (sy stepper), 
      customObjArg [] ode (customClass [
        constructorInfo [] [],
        methodInfo "operator()" [unnamedParam (List Float),
          unnamedParam (List Float), lockedParam t] Void [
            assignArrayIndex 0]]),
      -- Need to declare variable holding initial value because odeint will update this variable at each step
      preDefinedArg odeintCurrVals,
      inlineArg Float, inlineArg Float, inlineArg Float, 
      customObjArg [] pop (customClass [
        constructorInfo [unnamedParam (List Float)] [],
        methodInfo "operator()" [lockedParam y, lockedParam t] Void
          [appendCurrSol y]])]]

odeNameSpace, rkdp5, adamsBash :: String
odeNameSpace = "boost::numeric::odeint::"
rkdp5 = odeNameSpace ++ "runge_kutta_dopri5<vector<double>>"
adamsBash = odeNameSpace ++ "adams_bashforth<3,vector<double>>"

odeintCurrVals, rk, stepper, pop, rkdp5C, makeControlled, adamsBashC, 
  integrateConst :: CodeChunk
odeintCurrVals = codevar $ implCQD "currVals_odeint" (nounPhrase 
  "vector holding ODE solution values for the current step"
  "vectors holding ODE solution values for the current step") Nothing
  (List Float) (Label "currVals") Nothing
rk = codevar $ implCQD "rk_odeint" (nounPhrase 
  "stepper for solving ODE system using Runge-Kutta-Dopri5 method"
  "steppers for solving ODE system using Runge-Kutta-Dopri5 method") Nothing
  (Object rkdp5) (Label "rk") Nothing
stepper = codevar $ implCQD "stepper_odeint" (nounPhrase 
  "stepper for solving ODE system" "steppers for solving ODE system") Nothing
  (Object "auto") (Label "stepper") Nothing
pop = codevar $ implCQD "pop_odeint" (nounPhrase 
  "object to populate ODE solution vector" 
  "objects to populate ODE solution vector") Nothing (Object "Populate") 
  (Label "pop") Nothing
rkdp5C = codefunc $ implCQD "rkdp5_odeint" (nounPhrase
  "constructor for stepper using Runge-Kutta-Dopri5 method"
  "constructors for stepper using Runge-Kutta-Dopri5 method")
  Nothing (Object rkdp5) (Label rkdp5) Nothing
makeControlled = codefunc $ implCQD "make_controlled_odeint" (nounPhrase
  "function for adding error control to a stepper"
  "functions for adding error control to a stepper")
  Nothing (Object "auto") (Label $ odeNameSpace ++ "make_controlled") Nothing
adamsBashC = codefunc $ implCQD "adamsBash_odeint" (nounPhrase
  "constructor for stepper using Adams-Bashforth method"
  "constructors for stepper using Adams-Bashforth method")
  Nothing (Object adamsBash) (Label adamsBash) Nothing
integrateConst = codefunc $ implCQD "integrate_const_odeint" (nounPhrase
  "function for integrating with a constant step size"
  "functions for integrating with a constant step size")
  Nothing Void (Label $ odeNameSpace ++ "integrate_const") Nothing

-- CodeChunks used in multiple external libraries --

ode, t, y :: CodeChunk
ode = codevar $ implCQD "ode_obj" (nounPhrase 
  "object representing an ODE system" "objects representing an ODE system")
  Nothing (Object "ODE") (Label "ode") Nothing
t = codevar $ implCQD "t_ode" (nounPhrase 
  "current independent variable value in ODE solution"
  "current independent variable value in ODE solution") 
  Nothing Float (Label "t") Nothing
y = codevar $ implCQD "y_ode" (nounPhrase 
  "current dependent variable value in ODE solution"
  "current dependent variable value in ODE solution") 
  Nothing (List Float) (Label "y") Nothing
