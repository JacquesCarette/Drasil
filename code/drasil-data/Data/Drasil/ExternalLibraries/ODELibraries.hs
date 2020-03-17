module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODEPckg, scipyODESymbols, osloPckg, osloSymbols, apacheODEPckg, 
  apacheODESymbols, odeintPckg, odeintSymbols
) where

import Language.Drasil

import Language.Drasil.Code (Lang(..), ExternalLibrary, Step, Argument, 
  externalLib, mandatoryStep, mandatorySteps, choiceSteps, choiceStep,
  callStep, libFunction, libMethod, libFunctionWithResult, libMethodWithResult, 
  libConstructor, libConstructorMultiReqs, constructAndReturn, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, functionArg, 
  customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, methodInfoNoReturn, 
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj, 
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile, 
  returnExprList, fixedReturn,
  ExternalLibraryCall, externalLibCall, choiceStepsFill, choiceStepFill, 
  mandatoryStepFill, mandatoryStepsFill, callStepFill, libCallFill, 
  userDefinedArgFill, basicArgFill, functionArgFill, customObjArgFill, 
  recordArgFill, unnamedParamFill, userDefinedParamFill, customClassFill, 
  implementationFill, constructorInfoFill, methodInfoFill, appendCurrSolFill, 
  populateSolListFill, assignArrayIndexFill, assignSolFromObjFill, 
  initSolListFromArrayFill, initSolListWithValFill, solveAndPopulateWhileFill, 
  returnExprListFill, fixedStatementFill, CodeVarChunk, CodeFuncChunk, codevar, 
  codefunc, ccObjVar, implCQD, ODEInfo(..), ODEOptions(..), ODEMethod(..), 
  ODELibPckg, mkODELib, pubStateVar, privStateVar)

import Control.Lens ((^.))

-- SciPy -- 

scipyODEPckg :: ODELibPckg
scipyODEPckg = mkODELib scipyODE scipyCall [Python]

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ callStep $ libFunctionWithResult scipyImport
    odefunc [
      functionArg f (map unnamedParam [Real, Array Real]) 
      returnExprList] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatorySteps [callStep $ libMethod scipyImport r 
      setInitVal [inlineArg Real, inlineArg Real],
    initSolListWithVal,
    solveAndPopulateWhile (libMethod scipyImport r successful []) rt 
      (libMethod scipyImport r integrateStep [inlineArg Real]) ry]]

scipyCall :: ODEInfo -> ExternalLibraryCall
scipyCall info = externalLibCall [
  mandatoryStepFill $ callStepFill $ libCallFill [functionArgFill 
    (map unnamedParamFill [indepVar info, depVar info]) 
    (returnExprListFill $ odeSyst info)],
  uncurry choiceStepFill (chooseMethod $ solveMethod $ odeOpts info),
  mandatoryStepsFill [callStepFill $ libCallFill $ map basicArgFill 
      [initVal info, tInit info],
    initSolListWithValFill (depVar info) (initVal info),
    solveAndPopulateWhileFill (libCallFill []) (tFinal info) 
      (libCallFill [basicArgFill (sy rt + stepSize (odeOpts info))]) 
      (depVar info)]]
  where chooseMethod Adams = (0, solveMethodFill)
        chooseMethod BDF = (1, solveMethodFill)
        chooseMethod RK45 = (2, solveMethodFill)
        solveMethodFill = callStepFill $ libCallFill $ map basicArgFill 
          [absTol $ odeOpts info, relTol $ odeOpts info]

scipyImport :: String
scipyImport = "scipy.integrate"

atol, rtol, vode :: Argument
vode = lockedArg (str "vode")
atol = inlineNamedArg atolArg Real
rtol = inlineNamedArg rtolArg Real

methodArg :: String -> Argument
methodArg = lockedNamedArg mthdArg . str

setIntegratorMethod :: [Argument] -> Step
setIntegratorMethod = callStep . libMethod scipyImport r setIntegrator

odeT :: Space
odeT = Actor "ode"

scipyODESymbols :: [QuantityDict]
scipyODESymbols = map qw [mthdArg, atolArg, rtolArg] ++ map qw [r, t, y]
  ++ map qw [f, odefunc, setIntegrator, setInitVal, successful, integrateStep]

mthdArg, atolArg, rtolArg :: NamedArgument
mthdArg = narg $ implCQD "method_scipy" (nounPhrase 
  "chosen method for solving ODE" "chosen methods for solving ODE") 
  Nothing String (Label "method") Nothing
atolArg = narg $ implCQD "atol_scipy" (nounPhrase 
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution") 
  Nothing Real (Label "atol") Nothing
rtolArg = narg $ implCQD "rtol_scipy" (nounPhrase 
  "relative tolerance for ODE solution" "relative tolerances for ODE solution") 
  Nothing Real (Label "rtol") Nothing

r, rt, ry :: CodeVarChunk
r = codevar $ implCQD "r_scipy" (nounPhrase "ODE object" "ODE objects") Nothing 
  odeT (Label "r") Nothing
rt = ccObjVar r t
ry = ccObjVar r y

f, odefunc, setIntegrator, setInitVal, successful, 
  integrateStep :: CodeFuncChunk
f = codefunc $ implCQD "f_scipy" (nounPhrase "function representing ODE system" 
  "functions representing ODE system") Nothing (Array Real) (Label "f") Nothing
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
  Nothing Boolean (Label "successful") Nothing
integrateStep = codefunc $ implCQD "integrate_scipy" (nounPhrase
  "method that performs one integration step on an ODE"
  "methods that perform one integration step on an ODE")
  Nothing Void (Label "integrate") Nothing


-- Oslo (C#) --

osloPckg :: ODELibPckg
osloPckg = mkODELib oslo osloCall [CSharp]

oslo :: ExternalLibrary
oslo = externalLib [
  mandatoryStep $ callStep $ libConstructor osloImport 
    vector [inlineArg Real] initv,
  choiceStep $ map (\s -> callStep $ libFunctionWithResult osloImport s odeArgs 
    sol) [rk547m, gearBDF],
  mandatorySteps (callStep (libMethodWithResult osloImport sol 
      solveFromToStep (map inlineArg [Real, Real, Real]) points) :
    populateSolList points sp x)]

osloCall :: ODEInfo -> ExternalLibraryCall
osloCall info = externalLibCall [
  mandatoryStepFill $ callStepFill $ libCallFill [basicArgFill $ initVal info],
  choiceStepFill (chooseMethod $ solveMethod $ odeOpts info) $ callStepFill $ 
    libCallFill [basicArgFill $ tInit info, 
      functionArgFill (map unnamedParamFill [indepVar info, depVar info]) $ 
        callStepFill $ libCallFill $ map userDefinedArgFill (odeSyst info), 
      recordArgFill [absTol $ odeOpts info, relTol $ odeOpts info]],
  mandatoryStepsFill (callStepFill (libCallFill $ map basicArgFill 
      [tInit info, tFinal info, stepSize $ odeOpts info]) :
    populateSolListFill (depVar info))]
  where chooseMethod RK45 = 0
        chooseMethod BDF = 1
        chooseMethod _ = error odeMethodUnavailable

odeArgs :: [Argument]
odeArgs = [inlineArg Real, lockedArg (sy initv),
  functionArg fOslo (map unnamedParam [Real, vecT]) 
    (callStep $ constructAndReturn osloImport vector []),
  recordArg options opts [aTol, rTol]]

solT, vecT, optT :: Space
solT = Actor "IEnumerable<SolPoint>"
vecT = Actor "Vector"
optT = Actor "Options"

osloImport :: String
osloImport = "Microsoft.Research.Oslo"

osloSymbols :: [QuantityDict]
osloSymbols = map qw [initv, opts, aTol, rTol, sol, points, sp, x] ++ 
  map qw [fOslo, options, vector, rk547m, gearBDF, solveFromToStep]

initv, opts, aTol, rTol, sol, points, sp, x :: CodeVarChunk
initv = codevar $ implCQD "initv_oslo" (nounPhrase 
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables") Nothing
  vecT (Label "initv") Nothing
opts = codevar $ implCQD "opts_oslo" (nounPhrase 
  "record containing options for ODE solving" 
  "records containing options for ODE solving") Nothing optT 
  (Label "opts") Nothing
aTol = codevar $ implCQD "aTol_oslo" (nounPhrase 
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution")
  Nothing Real (Label "AbsoluteTolerance") Nothing
rTol = codevar $ implCQD "rTol_oslo" (nounPhrase 
  "relative tolerance for ODE solution" "relative tolerances for ODE solution")
  Nothing Real (Label "RelativeTolerance") Nothing
sol = codevar $ implCQD "sol_oslo" (nounPhrase "container for ODE information" 
  "containers for ODE information") Nothing solT (Label "sol") Nothing
points = codevar $ implCQD "points_oslo" (nounPhrase 
  "container holding ODE solution" "containers holding ODE solution") Nothing
  solT (Label "points") Nothing
sp = codevar $ implCQD "sp_oslo" (nounPhrase "ODE solution point" 
  "ODE solution points") Nothing (Actor "SolPoint") (Label "sp") Nothing
x = codevar $ implCQD "X_oslo" (nounPhrase "dependent variable" 
  "dependent variables") Nothing (Array Real) (Label "X") Nothing

fOslo, options, vector, rk547m, gearBDF, solveFromToStep :: CodeFuncChunk
fOslo = codefunc $ implCQD "f_oslo" (nounPhrase 
  "function representing ODE system" "functions representing ODE system") 
  Nothing vecT (Label "f") Nothing
options = codefunc $ implCQD "Options_oslo" (nounPhrase 
  "constructor for Options record" "constructors for Options record")
  Nothing optT (Label "Options") Nothing
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

apacheODEPckg :: ODELibPckg
apacheODEPckg = mkODELib apacheODE apacheODECall [Java]

apacheODE :: ExternalLibrary
apacheODE = externalLib [
  choiceStep [
    callStep $ libConstructorMultiReqs [apacheImport ++ "nonstiff." ++ adams,
      foiImp] adamsC (lockedArg (int 3) : itArgs) it,
    callStep $ libConstructorMultiReqs [apacheImport ++ "nonstiff." ++ dp54,
      foiImp] dp54C itArgs it],
  mandatorySteps [callStep $ libMethod foiImp it addStepHandler [
      customObjArg [shImp, siImp]
        "Class defining additional behaviour for each step of an ODE solution"
        stepHandler stepHandlerCtor (implementation sh [
          methodInfoNoReturn initMethod 
            "initializes step handler with initial conditions" 
            (map lockedParam [t0, y0, t]) [initSolListFromArray y0],
          methodInfoNoReturn handleStep 
            "appends solution point at each ODE solution step"
            (map lockedParam [interpolator, isLast]) 
            [callStep $ libMethodWithResult siImp interpolator getInterpState 
              [] curr,
            appendCurrSol curr]])],
    callStep $ libMethod foiImp it integrate (customObjArg [apacheImport ++ 
      fode] "Class representing an ODE system" ode odeCtor (implementation fode 
        [constructorInfo odeCtor [] [],
        methodInfo getDimension "returns the ODE system dimension" 
          [] "dimension of the ODE system" [fixedReturn (int 1)],
        methodInfoNoReturn computeDerivatives 
          "function representation of an ODE system" 
          [lockedParam t, unnamedParam (Array Real), unnamedParam (Array Real)]
          [assignArrayIndex]]) : 
      [inlineArg Real, preDefinedArg currVals, inlineArg Real, 
        preDefinedArg currVals]),
    assignSolFromObj stepHandler]]

apacheODECall :: ODEInfo -> ExternalLibraryCall
apacheODECall info = externalLibCall [
  choiceStepFill (chooseMethod $ solveMethod $ odeOpts info) $ callStepFill $ 
    libCallFill (map (basicArgFill . ($ odeOpts info)) 
      [stepSize, stepSize, absTol, relTol]),
  mandatoryStepsFill [callStepFill $ libCallFill [
      customObjArgFill [pubStateVar $ depVar info] (implementationFill [
        methodInfoFill [] [initSolListFromArrayFill $ depVar info], methodInfoFill [] 
          [callStepFill $ libCallFill [], appendCurrSolFill $ depVar info]])],
    callStepFill $ libCallFill $ customObjArgFill 
      (map privStateVar $ otherVars info) 
      (implementationFill [
        constructorInfoFill (map userDefinedParamFill $ otherVars info) 
          (zip (otherVars info) (map sy $ otherVars info)) [], 
        methodInfoFill [] [fixedStatementFill], 
        methodInfoFill (map unnamedParamFill [depVar info, ddep]) 
          [assignArrayIndexFill ddep (odeSyst info)]]) 
      : map basicArgFill [tInit info, Matrix [[initVal info]], tFinal info, 
        Matrix [[initVal info]]],
    assignSolFromObjFill $ depVar info]]
  where chooseMethod Adams = 0
        chooseMethod RK45 = 1
        chooseMethod _ = error odeMethodUnavailable
        ddep = diffCodeChunk $ depVar info

itArgs :: [Argument]
itArgs = map inlineArg [Real, Real, Real, Real]

apacheImport, adams, dp54, foi, foiImp, sampling, sh, shImp, si, siImp, fode :: String
apacheImport = "org.apache.commons.math3.ode."
adams = "AdamsBashforthIntegrator"
dp54 = "DormandPrince54Integrator"
foi = "FirstOrderIntegrator"
foiImp = apacheImport ++ foi
sampling = "sampling"
sh = "StepHandler"
shImp = apacheImport ++ sampling ++ "." ++ sh
si = "StepInterpolator"
siImp = apacheImport ++ sampling ++ "." ++ si
fode = "FirstOrderDifferentialEquations"

apacheODESymbols :: [QuantityDict]
apacheODESymbols = map qw [it, currVals, stepHandler, t0, y0, t, interpolator, 
  isLast, curr, ode] ++ map qw [adamsC, dp54C, stepHandlerCtor, addStepHandler, 
  initMethod, handleStep, getInterpState, integrate, odeCtor, getDimension, 
  computeDerivatives]

it, currVals, stepHandler, t0, y0, interpolator, isLast, curr :: CodeVarChunk
it = codevar $ implCQD "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") Nothing (Actor foi) (Label "it") Nothing
currVals = codevar $ implCQD "curr_vals_apache" (nounPhrase 
  "array holding ODE solution values for the current step"
  "arrays holding ODE solution values for the current step") Nothing 
  (Array Real) (Label "curr_vals") Nothing
stepHandler = codevar $ implCQD "stepHandler_apache" (nounPhrase 
  "ODE step handler" "ODE step handlers") Nothing (Actor sh)
  (Label "stepHandler") Nothing
t0 = codevar $ implCQD "t0_apache" (nounPhrase "initial time for ODE solving"
  "intial times for ODE solving") Nothing Real (Label "t0") Nothing
y0 = codevar $ implCQD "y0_apache" (nounPhrase 
  "array of initial values for ODE solving" 
  "arrays of initial values for ODE solving") 
  Nothing (Array Real) (Label "y0") Nothing
interpolator = codevar $ implCQD "interpolator_apache" (nounPhrase 
  "step interpolator for ODE solving" "step interpolator for ODE solving")
  Nothing (Actor si) (Label "interpolator") Nothing
isLast = codevar $ implCQD "isLast_apache" (nounPhrase 
  "boolean for whether the current step is the last step"
  "booleans for whether the current step is the last step")
  Nothing Boolean (Label "isLast") Nothing
curr = codevar $ implCQD "curr_apache" (nounPhrase 
  "ODE solution array for current step" "ODE solution arrays for current step")
  Nothing (Array Real) (Label "curr") Nothing

adamsC, dp54C, stepHandlerCtor, addStepHandler, initMethod, handleStep, 
  getInterpState, integrate, getDimension, computeDerivatives :: CodeFuncChunk
adamsC = codefunc $ implCQD "adams_ctor_apache" (nounPhrase
  "constructor for an Adams-Bashforth integrator" 
  "constructors for an Adams-Bashforth integrator") 
  Nothing (Actor adams) (Label adams) Nothing
dp54C = codefunc $ implCQD "dp54_ctor_apache" (nounPhrase
  "constructor for a Dormand-Prince 5-4 integrator"
  "constructors for a Dormand-Prince 5-4 integrator")
  Nothing (Actor dp54) (Label dp54) Nothing
stepHandlerCtor = codefunc $ implCQD "StepHandler_ctor_apache" (nounPhrase
  "constructor for StepHandler" "constructors for StepHandler") Nothing 
  (Actor sh) (Label "StepHandler") Nothing
addStepHandler = codefunc $ implCQD "addStepHandler_apache" (nounPhrase
  "method for adding a step handler to an integrator"
  "methods for adding a step handler to an integrator")
  Nothing Void (Label "addStepHandler") Nothing
initMethod = codefunc $ implCQD "init_apache" (nounPhrase 
  "method to initialize step handler" "methods to initialize step handler")
  Nothing Void (Label "init") Nothing
handleStep = codefunc $ implCQD "handleStep_apache" (nounPhrase
  "method to call at each ODE step" "methods to call at each ODE step") 
  Nothing Void (Label "handleStep") Nothing
getInterpState = codefunc $ implCQD "getInterpolatedState_apache" (nounPhrase
  "method for getting current state during ODE solving"
  "methods for getting current state during ODE solving")
  Nothing (Array Real) (Label "getInterpolatedState") Nothing
integrate = codefunc $ implCQD "integrate_apache" (nounPhrase
  "method for integrating an ODE" "methods for integrating an ODE")
  Nothing Void (Label "integrate") Nothing
getDimension = codefunc $ implCQD "getDimension_apache" (nounPhrase
  "method returning the dimension of an ODE system"
  "methods returning the dimension of an ODE system")
  Nothing Natural (Label "getDimension") Nothing
computeDerivatives = codefunc $ implCQD "computeDerivatives_apache" (nounPhrase 
  "method encoding an ODE system" "methods encoding an ODE system")
  Nothing Void (Label "computeDerivatives") Nothing

-- odeint (C++) --

odeintPckg :: ODELibPckg
odeintPckg = mkODELib odeint odeintCall [Cpp]

odeint :: ExternalLibrary
odeint = externalLib [
  choiceSteps [
    [callStep $ libConstructor odeintImport rkdp5C [] rk,
    callStep $ libFunctionWithResult odeintImport makeControlled 
      [inlineArg Real, inlineArg Real, lockedArg (sy rk)] stepper],
    [callStep $ libConstructor odeintImport adamsBashC [] stepper]],
  mandatoryStep $ callStep $ libFunction odeintImport
    integrateConst [
      lockedArg (sy stepper), 
      customObjArg [] "Class representing an ODE system" ode odeCtor 
        (customClass [constructorInfo odeCtor [] [],
          methodInfoNoReturn odeOp "function representation of ODE system" 
            [unnamedParam (Vect Real), unnamedParam (Vect Real), lockedParam t] 
            [assignArrayIndex]]),
      -- Need to declare variable holding initial value because odeint will update this variable at each step
      preDefinedArg odeintCurrVals,
      inlineArg Real, inlineArg Real, inlineArg Real, 
      customObjArg [] 
        "Class for populating a list during an ODE solution process" 
        pop popCtor (customClass [
          constructorInfo popCtor [unnamedParam (Vect Real)] [],
          methodInfoNoReturn popOp 
            "appends solution point for current ODE solution step"
            [lockedParam y, lockedParam t] [appendCurrSol y]])]]

odeintCall :: ODEInfo -> ExternalLibraryCall
odeintCall info = externalLibCall [
  uncurry choiceStepsFill (chooseMethod $ solveMethod $ odeOpts info),
  mandatoryStepFill $ callStepFill $ libCallFill $
    customObjArgFill (map privStateVar $ otherVars info) (customClassFill [
      constructorInfoFill (map userDefinedParamFill $ otherVars info) 
        (zip (otherVars info) (map sy $ otherVars info)) [], 
      methodInfoFill (map unnamedParamFill [depVar info, ddep]) 
        [assignArrayIndexFill ddep (odeSyst info)]]) :
    map basicArgFill [Matrix [[initVal info]], tInit info, tFinal info, 
      stepSize $ odeOpts info] ++ [
    customObjArgFill [privStateVar $ depVar info] (customClassFill [
      constructorInfoFill [unnamedParamFill $ depVar info] 
        [(depVar info, sy $ depVar info)] [],
      methodInfoFill [] [appendCurrSolFill $ depVar info]])]]
  where chooseMethod RK45 = (0, map (callStepFill . libCallFill . map 
          basicArgFill) [[], [absTol $ odeOpts info, relTol $ odeOpts info]])
        chooseMethod Adams = (1, [callStepFill $ libCallFill []])
        chooseMethod _ = error odeMethodUnavailable
        ddep = diffCodeChunk $ depVar info

odeintImport, odeNameSpace, rkdp5, adamsBash :: String
odeintImport = "boost/numeric/odeint.hpp"
odeNameSpace = "boost::numeric::odeint::"
rkdp5 = odeNameSpace ++ "runge_kutta_dopri5<vector<double>>"
adamsBash = odeNameSpace ++ "adams_bashforth<3,vector<double>>"

popT :: Space
popT = Actor "Populate"

odeintSymbols :: [QuantityDict]
odeintSymbols = map qw [odeintCurrVals, rk, stepper, pop, t, y, ode] ++ map qw 
  [rkdp5C, makeControlled, adamsBashC, integrateConst, odeCtor, odeOp, popCtor, 
  popOp]

odeintCurrVals, rk, stepper, pop :: CodeVarChunk
odeintCurrVals = codevar $ implCQD "currVals_odeint" (nounPhrase 
  "vector holding ODE solution values for the current step"
  "vectors holding ODE solution values for the current step") Nothing
  (Vect Real) (Label "currVals") Nothing
rk = codevar $ implCQD "rk_odeint" (nounPhrase 
  "stepper for solving ODE system using Runge-Kutta-Dopri5 method"
  "steppers for solving ODE system using Runge-Kutta-Dopri5 method") Nothing
  (Actor rkdp5) (Label "rk") Nothing
stepper = codevar $ implCQD "stepper_odeint" (nounPhrase 
  "stepper for solving ODE system" "steppers for solving ODE system") Nothing
  (Actor "auto") (Label "stepper") Nothing
pop = codevar $ implCQD "pop_odeint" (nounPhrase 
  "object to populate ODE solution vector" 
  "objects to populate ODE solution vector") Nothing popT (Label "pop") Nothing

rkdp5C, makeControlled, adamsBashC, integrateConst, odeOp, popCtor, 
  popOp :: CodeFuncChunk
rkdp5C = codefunc $ implCQD "rkdp5_odeint" (nounPhrase
  "constructor for stepper using Runge-Kutta-Dopri5 method"
  "constructors for stepper using Runge-Kutta-Dopri5 method")
  Nothing (Actor rkdp5) (Label rkdp5) Nothing
makeControlled = codefunc $ implCQD "make_controlled_odeint" (nounPhrase
  "function for adding error control to a stepper"
  "functions for adding error control to a stepper")
  Nothing (Actor "auto") (Label $ odeNameSpace ++ "make_controlled") Nothing
adamsBashC = codefunc $ implCQD "adamsBash_odeint" (nounPhrase
  "constructor for stepper using Adams-Bashforth method"
  "constructors for stepper using Adams-Bashforth method")
  Nothing (Actor adamsBash) (Label adamsBash) Nothing
integrateConst = codefunc $ implCQD "integrate_const_odeint" (nounPhrase
  "function for integrating with a constant step size"
  "functions for integrating with a constant step size")
  Nothing Void (Label $ odeNameSpace ++ "integrate_const") Nothing
odeOp = codefunc $ implCQD "ode_operator_odeint" (nounPhrase 
  "method defining override for calling ODE object"
  "methods defining override for calling ODE object") Nothing Void 
  (Label "operator()") Nothing
popCtor = codefunc $ implCQD "Populate_odeint" (nounPhrase 
  "constructor for Populate object for ODE solving with odeint" 
  "constructors for Populate object for ODE solving with odeint")
  Nothing popT (Label "Populate") Nothing
popOp = codefunc $ implCQD "pop_operator_odeint" (nounPhrase
  "method defining override for calling Populate object"
  "methods defining override for calling Populate object") Nothing Void
  (Label "operator()") Nothing

-- CodeChunks used in multiple external libraries --

ode, t, y :: CodeVarChunk
ode = codevar $ implCQD "ode_obj" (nounPhrase 
  "object representing an ODE system" "objects representing an ODE system")
  Nothing odeObj (Label "ode") Nothing
t = codevar $ implCQD "t_ode" (nounPhrase 
  "current independent variable value in ODE solution"
  "current independent variable value in ODE solution") 
  Nothing Real (Label "t") Nothing
y = codevar $ implCQD "y_ode" (nounPhrase 
  "current dependent variable value in ODE solution"
  "current dependent variable value in ODE solution") 
  Nothing (Vect Real) (Label "y") Nothing

odeCtor :: CodeFuncChunk
odeCtor = codefunc $ implCQD "ODE_constructor" (nounPhrase
  "constructor for ODE object" "constructors for ODE object") Nothing odeObj
  (Label "ODE") Nothing

odeObj :: Space
odeObj = Actor "ODE"

odeMethodUnavailable :: String
odeMethodUnavailable = "Chosen ODE solving method is not available" ++
          " in chosen ODE solving library"

diffCodeChunk :: CodeVarChunk -> CodeVarChunk
diffCodeChunk c = codevar $ implCQD ("d" ++ c ^. uid) 
  (compoundPhrase (nounPhraseSP "change in") (c ^. term)) Nothing (c ^. typ)
  (Concat [Label "d", symbol c Implementation]) (getUnit c) 