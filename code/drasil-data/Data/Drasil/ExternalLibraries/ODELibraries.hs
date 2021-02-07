module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODEPckg, scipyODESymbols, osloPckg, osloSymbols, arrayVecDepVar, 
  apacheODEPckg, apacheODESymbols, odeintPckg, odeintSymbols, scipyODELSodaPkg
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
  returnExprList, fixedReturn, initSolWithVal,
  ExternalLibraryCall, externalLibCall, choiceStepsFill, choiceStepFill, 
  mandatoryStepFill, mandatoryStepsFill, callStepFill, libCallFill, 
  userDefinedArgFill, basicArgFill, functionArgFill, customObjArgFill, 
  recordArgFill, unnamedParamFill, unnamedParamPBVFill, userDefinedParamFill, 
  customClassFill, implementationFill, constructorInfoFill, methodInfoFill, 
  appendCurrSolFill, populateSolListFill, assignArrayIndexFill, 
  assignSolFromObjFill, initSolListFromArrayFill, initSolListWithValFill, 
  solveAndPopulateWhileFill, returnExprListFill, fixedStatementFill, 
  CodeVarChunk, CodeFuncChunk, quantvar, quantfunc, listToArray, 
  ODEInfo(..), ODEOptions(..), ODEMethod(..), ODELibPckg, mkODELib, 
  mkODELibNoPath, pubStateVar, privStateVar, field, initSolWithValFill)

import Control.Lens ((^.), _1, _2, over)

-- SciPy -- 

scipyODEPckg :: ODELibPckg
scipyODEPckg = mkODELibNoPath "SciPy" "1.4.1" scipyODE scipyCall [Python]

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ callStep $ libFunctionWithResult scipyImport
    odefunc [
      functionArg f (map unnamedParam [Real, Array Real]) 
      returnExprList] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri5"), atol, rtol]],
  mandatorySteps [callStep $ libMethod scipyImport r 
      setInitVal [inlineArg Real, inlineArg Real],
    initSolListWithVal,
    solveAndPopulateWhile (libMethod scipyImport r successful []) r t 
      (libMethod scipyImport r integrateStep [inlineArg Real]) y]]

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
      (libCallFill [basicArgFill (field r t + stepSize (odeOpts info))]) 
      (depVar info)]]
  where chooseMethod Adams = (0, solveMethodFill)
        chooseMethod BDF = (1, solveMethodFill)
        chooseMethod RK45 = (2, solveMethodFill)
        solveMethodFill = callStepFill $ libCallFill $ map basicArgFill 
          [absTol $ odeOpts info, relTol $ odeOpts info]

-- This package solves a system of ODE using the scipy odeint method.
-- The odeint method solves the ode using the LSoda solver.
scipyODELSodaPkg :: ODELibPckg
scipyODELSodaPkg = mkODELibNoPath "SciPy" "1.4.1" scipyLSodaODE scipyLSodaCall [Python]

scipyLSodaODE :: ExternalLibrary
scipyLSodaODE = externalLib [
  mandatoryStep $ callStep $ libFunctionWithResult numpyImport
    arange [inlineArg Real, inlineArg Real, inlineArg Real] xAxis,
  mandatoryStep $ callStep $ libFunctionWithResult scipyImport
    odeintFunc [
      functionArg f (map unnamedParam [Array Real, Real]) 
      returnExprList, inlineArg (Array Real), inlineArg (Array Real)] ut,
  mandatoryStep initSolWithVal
    ]

scipyLSodaCall :: ODEInfo -> ExternalLibraryCall
scipyLSodaCall info = externalLibCall [
  mandatoryStepsFill [callStepFill $ libCallFill $ map basicArgFill 
      [tInit info, tFinal info, stepSize $ odeOpts info]],
  mandatoryStepFill $ callStepFill $ libCallFill [functionArgFill 
      (map unnamedParamFill [depVar info, indepVar info]) 
      (returnExprListFill $ odeSyst info), 
      basicArgFill (Matrix [[initVal info, initValFstOrd $ odeOpts info]]),
      basicArgFill (sy xAxis)],
  mandatoryStepFill $ initSolWithValFill (depVar info) 
      (idx (sy transpose) (Int 0))
    ]

scipyImport :: String
scipyImport = "scipy.integrate"

numpyImport :: String
numpyImport = "numpy"

atol, rtol, vode :: Argument
vode = lockedArg (str "vode")
atol = inlineNamedArg atolArg Real
rtol = inlineNamedArg rtolArg Real

methodArg :: String -> Argument
methodArg = lockedNamedArg mthdArg . str

setIntegratorMethod :: [Argument] -> Step
setIntegratorMethod = callStep . libMethod scipyImport r setIntegrator

odeT, numpyArrayT :: Space
odeT = Actor "ode"
numpyArrayT = Actor "numpyArray"

scipyODESymbols :: [QuantityDict]
scipyODESymbols = map qw [mthdArg, atolArg, rtolArg] 
  ++ map qw [r, t, y, xAxis, ut, transpose]
  ++ map qw [f, odefunc, setIntegrator, setInitVal, successful, integrateStep,
  arange, odeintFunc]

mthdArg, atolArg, rtolArg :: NamedArgument
mthdArg = narg $ implVar "method_scipy" (nounPhrase 
  "chosen method for solving ODE" "chosen methods for solving ODE") 
  String (Label "method")
atolArg = narg $ implVar "atol_scipy" (nounPhrase 
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution") 
  Real (Label "atol")
rtolArg = narg $ implVar "rtol_scipy" (nounPhrase 
  "relative tolerance for ODE solution" "relative tolerances for ODE solution") 
  Real (Label "rtol")


r, xAxis, ut, transpose :: CodeVarChunk
r = quantvar $ implVar "r_scipy" (nounPhrase "ODE object" "ODE objects") 
  odeT (Label "r")
xAxis = quantvar $ implVar "x_numpy" (nounPhrase "Numpy value" "Numpy value") 
  (Array Real) (Label "x_axis")
ut = quantvar $ implVar "ut_scipy" 
  (nounPhrase "Scipy integrated value" "Scipy integrated value") 
  numpyArrayT (Label "u_t")
transpose = quantvar $ implVar "transpose_numpy" 
  (nounPhrase "Numpy Array Transpose" "Numpy Array Transpose") 
  (Array Real) (Label "u_t.T") -- (ccObjVar ut transpose) does not seem to work. 


f, odefunc, setIntegrator, setInitVal, successful, 
  integrateStep, arange, odeintFunc :: CodeFuncChunk
f = quantfunc $ implVar "f_scipy" (nounPhrase "function representing ODE system" 
  "functions representing ODE system") (Array Real) (Label "f")
odefunc = quantfunc $ implVar "ode_scipy" (nounPhrase 
  "function for defining an ODE for SciPy" 
  "functions for defining an ODE for SciPy") odeT (Label "ode")
setIntegrator = quantfunc $ implVar "set_integrator_scipy" (nounPhrase
  "method for setting SciPy integrator" "methods for setting SciPy integrator")
  Void (Label "set_integrator")
setInitVal = quantfunc $ implVar "set_initial_value_scipy" (nounPhrase
  "method for setting initial value for ODE for SciPy" 
  "methods for setting initial value for ODE for SciPy")
  Void (Label "set_initial_value")
successful = quantfunc $ implVar "successful_scipy" (nounPhrase 
  "method returning True if integration is current successful"
  "methods returning True if integration is current successful")
  Boolean (Label "successful")
integrateStep = quantfunc $ implVar "integrate_scipy" (nounPhrase
  "method that performs one integration step on an ODE"
  "methods that perform one integration step on an ODE") 
  Void (Label "integrate")
arange = quantfunc $ implVar "arrange_numpy" (nounPhrase
  "method that returns evenly spaced numbers over a specified interval."
  "method that returns evenly spaced numbers over a specified interval.")
  (Array Real) (Label "arange")
odeintFunc = quantfunc $ implVar "odeint_scipy" (nounPhrase
  "method that solves a system of ODE using lsoda from the FORTRAN library odepack."
  "method that solves a system of ODE using lsoda from the FORTRAN library odepack.")
  (Array Real) (Label "odeint")

-- Oslo (C#) --

osloPckg :: ODELibPckg
osloPckg = mkODELib "OSLO" "1.2" oslo osloCall "Microsoft.Research.Oslo.dll" [CSharp]

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
      functionArgFill (map unnamedParamFill [indepVar info, vecDepVar info]) $ 
        callStepFill $ libCallFill $ map userDefinedArgFill (modifiedODESyst "arrayvec" info),
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
  recordArg osloImport options opts [aTol, rTol]]

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
initv = quantvar $ implVar "initv_oslo" (nounPhrase 
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables")
  vecT (Label "initv")
opts = quantvar $ implVar "opts_oslo" (nounPhrase 
  "record containing options for ODE solving" 
  "records containing options for ODE solving") optT (Label "opts")
aTol = quantvar $ implVar "aTol_oslo" (nounPhrase 
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution")
  Real (Label "AbsoluteTolerance")
rTol = quantvar $ implVar "rTol_oslo" (nounPhrase 
  "relative tolerance for ODE solution" "relative tolerances for ODE solution")
  Real (Label "RelativeTolerance")
sol = quantvar $ implVar "sol_oslo" (nounPhrase "container for ODE information" 
  "containers for ODE information") solT (Label "sol")
points = quantvar $ implVar "points_oslo" (nounPhrase 
  "container holding ODE solution" "containers holding ODE solution")
  solT (Label "points")
sp = quantvar $ implVar "sp_oslo" (nounPhrase "ODE solution point" 
  "ODE solution points") (Actor "SolPoint") (Label "sp")
x = quantvar $ implVar "X_oslo" (nounPhrase "dependent variable" 
  "dependent variables") (Array Real) (Label "X")

fOslo, options, vector, rk547m, gearBDF, solveFromToStep :: CodeFuncChunk
fOslo = quantfunc $ implVar "f_oslo" (nounPhrase 
  "function representing ODE system" "functions representing ODE system") 
  vecT (Label "f")
options = quantfunc $ implVar "Options_oslo" (nounPhrase 
  "constructor for Options record" "constructors for Options record")
  optT (Label "Options")
vector = quantfunc $ implVar "Vector_oslo" (nounPhrase 
  "constructor for an OSLO Vector" "constructors for an OSLO Vector")
  vecT (Label "Vector")
rk547m = quantfunc $ implVar "RK547M_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Runge-Kutta method"
  "functions for initiating an ODE to be solved by Runge-Kutta method")
  solT (Label "Ode.RK547M")
gearBDF = quantfunc $ implVar "GearBDF_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Gear's BDF method"
  "functions for initiating an ODE to be solved by Gear's BDF method")
  solT (Label "Ode.GearBDF")
solveFromToStep = quantfunc $ implVar "SolveFromToStep_oslo" (nounPhrase
  "method for solving an ODE given a time range" 
  "methods for solving an ODE given a time range")
  solT (Label "SolveFromToStep")

vecDepVar :: ODEInfo -> CodeVarChunk
vecDepVar info = quantvar $ implVar (dv ^. uid) (dv ^. term) vecT 
  (sub (symbol dv Implementation) (Label "vec"))
  where dv = depVar info

-- Hack required because Oslo's Vector type behaves like an array, so needs to
-- be represented as one or else will hit type errors in GOOL.
arrayVecDepVar :: ODEInfo -> CodeVarChunk
arrayVecDepVar info = quantvar $ implVar (dv ^. uid ++ "vec") (dv ^. term) 
  (dv ^. typ) (sub (symbol dv Implementation) (Label "vec")) 
  where dv = listToArray $ depVar info


-- Apache (Java) --

apacheODEPckg :: ODELibPckg
apacheODEPckg = mkODELib "Apache" "3.6.1" apacheODE apacheODECall 
  "lib/commons-math3-3.6.1.jar" [Java]

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
            appendCurrSol (sy curr)]])],
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
        methodInfoFill (map (unnamedParamFill . listToArray) [depVar info, ddep]) 
          [assignArrayIndexFill (listToArray ddep) (modifiedODESyst "array" info)]]) 
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
it = quantvar $ implVar "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") (Actor foi) (Label "it")
currVals = quantvar $ implVar "curr_vals_apache" (nounPhrase 
  "array holding ODE solution values for the current step"
  "arrays holding ODE solution values for the current step") 
  (Array Real) (Label "curr_vals")
stepHandler = quantvar $ implVar "stepHandler_apache" (nounPhrase 
  "ODE step handler" "ODE step handlers") (Actor $ "ODE" ++ sh)
  (Label "stepHandler")
t0 = quantvar $ implVar "t0_apache" (nounPhrase "initial time for ODE solving"
  "intial times for ODE solving") Real (Label "t0")
y0 = quantvar $ implVar "y0_apache" (nounPhrase 
  "array of initial values for ODE solving" 
  "arrays of initial values for ODE solving") (Array Real) (Label "y0")
interpolator = quantvar $ implVar "interpolator_apache" (nounPhrase 
  "step interpolator for ODE solving" "step interpolator for ODE solving")
  (Actor si) (Label "interpolator")
isLast = quantvar $ implVar "isLast_apache" (nounPhrase 
  "boolean for whether the current step is the last step"
  "booleans for whether the current step is the last step")
  Boolean (Label "isLast")
curr = quantvar $ implVar "curr_apache" (nounPhrase 
  "ODE solution array for current step" "ODE solution arrays for current step")
  (Array Real) (Label "curr")

adamsC, dp54C, stepHandlerCtor, addStepHandler, initMethod, handleStep, 
  getInterpState, integrate, getDimension, computeDerivatives :: CodeFuncChunk
adamsC = quantfunc $ implVar "adams_ctor_apache" (nounPhrase
  "constructor for an Adams-Bashforth integrator" 
  "constructors for an Adams-Bashforth integrator") (Actor adams) (Label adams)
dp54C = quantfunc $ implVar "dp54_ctor_apache" (nounPhrase
  "constructor for a Dormand-Prince 5-4 integrator"
  "constructors for a Dormand-Prince 5-4 integrator")
  (Actor dp54) (Label dp54)
stepHandlerCtor = quantfunc $ implVar "StepHandler_ctor_apache" (nounPhrase
  "constructor for StepHandler" "constructors for StepHandler") 
  (Actor $ "ODE" ++ sh) (Label $ "ODE" ++ sh)
addStepHandler = quantfunc $ implVar "addStepHandler_apache" (nounPhrase
  "method for adding a step handler to an integrator"
  "methods for adding a step handler to an integrator")
  Void (Label "addStepHandler")
initMethod = quantfunc $ implVar "init_apache" (nounPhrase 
  "method to initialize step handler" "methods to initialize step handler")
  Void (Label "init")
handleStep = quantfunc $ implVar "handleStep_apache" (nounPhrase
  "method to call at each ODE step" "methods to call at each ODE step") 
  Void (Label "handleStep")
getInterpState = quantfunc $ implVar "getInterpolatedState_apache" (nounPhrase
  "method for getting current state during ODE solving"
  "methods for getting current state during ODE solving")
  (Array Real) (Label "getInterpolatedState")
integrate = quantfunc $ implVar "integrate_apache" (nounPhrase
  "method for integrating an ODE" "methods for integrating an ODE")
  Void (Label "integrate")
getDimension = quantfunc $ implVar "getDimension_apache" (nounPhrase
  "method returning the dimension of an ODE system"
  "methods returning the dimension of an ODE system")
  Natural (Label "getDimension")
computeDerivatives = quantfunc $ implVar "computeDerivatives_apache" (nounPhrase 
  "method encoding an ODE system" "methods encoding an ODE system")
  Void (Label "computeDerivatives")

-- odeint (C++) --

odeintPckg :: ODELibPckg
odeintPckg = mkODELib "odeint" "v2" odeint odeintCall "." [Cpp]

odeint :: ExternalLibrary
odeint = externalLib [
  choiceSteps [
    [callStep $ libConstructor (odeintImport ++ "/stepper/runge_kutta_dopri5") rkdp5C [] rk,
    callStep $ libFunctionWithResult (odeintImport ++ "/stepper/generation") makeControlled 
      [inlineArg Real, inlineArg Real, lockedArg (sy rk)] stepper],
    [callStep $ libConstructor (odeintImport ++ "/stepper/adams_bashforth") adamsBashC [] stepper]],
  mandatoryStep $ callStep $ libFunction (odeintImport ++ "/integrate/integrate_const")
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
            [lockedParam y, lockedParam t] [appendCurrSol (sy y)]])]]

odeintCall :: ODEInfo -> ExternalLibraryCall
odeintCall info = externalLibCall [
  uncurry choiceStepsFill (chooseMethod $ solveMethod $ odeOpts info),
  mandatoryStepFill $ callStepFill $ libCallFill $
    customObjArgFill (map privStateVar $ otherVars info) (customClassFill [
      constructorInfoFill (map userDefinedParamFill $ otherVars info) 
        (zip (otherVars info) (map sy $ otherVars info)) [], 
      methodInfoFill [unnamedParamPBVFill $ depVar info, unnamedParamFill ddep]
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
odeintImport = "boost/numeric/odeint"
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
odeintCurrVals = quantvar $ implVar "currVals_odeint" (nounPhrase 
  "vector holding ODE solution values for the current step"
  "vectors holding ODE solution values for the current step")
  (Vect Real) (Label "currVals")
rk = quantvar $ implVar "rk_odeint" (nounPhrase 
  "stepper for solving ODE system using Runge-Kutta-Dopri5 method"
  "steppers for solving ODE system using Runge-Kutta-Dopri5 method")
  (Actor rkdp5) (Label "rk")
stepper = quantvar $ implVar "stepper_odeint" (nounPhrase 
  "stepper for solving ODE system" "steppers for solving ODE system")
  (Actor "auto") (Label "stepper")
pop = quantvar $ implVar "pop_odeint" (nounPhrase 
  "object to populate ODE solution vector" 
  "objects to populate ODE solution vector") popT (Label "pop")

rkdp5C, makeControlled, adamsBashC, integrateConst, odeOp, popCtor, 
  popOp :: CodeFuncChunk
rkdp5C = quantfunc $ implVar "rkdp5_odeint" (nounPhrase
  "constructor for stepper using Runge-Kutta-Dopri5 method"
  "constructors for stepper using Runge-Kutta-Dopri5 method")
  (Actor rkdp5) (Label rkdp5)
makeControlled = quantfunc $ implVar "make_controlled_odeint" (nounPhrase
  "function for adding error control to a stepper"
  "functions for adding error control to a stepper")
  (Actor "auto") (Label $ odeNameSpace ++ "make_controlled")
adamsBashC = quantfunc $ implVar "adamsBash_odeint" (nounPhrase
  "constructor for stepper using Adams-Bashforth method"
  "constructors for stepper using Adams-Bashforth method")
  (Actor adamsBash) (Label adamsBash)
integrateConst = quantfunc $ implVar "integrate_const_odeint" (nounPhrase
  "function for integrating with a constant step size"
  "functions for integrating with a constant step size")
  Void (Label $ odeNameSpace ++ "integrate_const")
odeOp = quantfunc $ implVar "ode_operator_odeint" (nounPhrase 
  "method defining override for calling ODE object"
  "methods defining override for calling ODE object") Void 
  (Label "operator()")
popCtor = quantfunc $ implVar "Populate_odeint" (nounPhrase 
  "constructor for Populate object for ODE solving with odeint" 
  "constructors for Populate object for ODE solving with odeint")
  popT (Label "Populate")
popOp = quantfunc $ implVar "pop_operator_odeint" (nounPhrase
  "method defining override for calling Populate object"
  "methods defining override for calling Populate object") Void
  (Label "operator()")

-- CodeChunks used in multiple external libraries --

ode, t, y :: CodeVarChunk
ode = quantvar $ implVar "ode_obj" (nounPhrase 
  "object representing an ODE system" "objects representing an ODE system")
  odeObj (Label "ode")
t = quantvar $ implVar "t_ode" (nounPhrase 
  "current independent variable value in ODE solution"
  "current independent variable value in ODE solution") 
  Real (Label "t")
y = quantvar $ implVar "y_ode" (nounPhrase 
  "current dependent variable value in ODE solution"
  "current dependent variable value in ODE solution") 
  (Vect Real) (Label "y")

odeCtor :: CodeFuncChunk
odeCtor = quantfunc $ implVar "ODE_constructor" (nounPhrase
  "constructor for ODE object" "constructors for ODE object") odeObj
  (Label "ODE")

odeObj :: Space
odeObj = Actor "ODE"

odeMethodUnavailable :: String
odeMethodUnavailable = "Chosen ODE solving method is not available" ++
          " in chosen ODE solving library"

diffCodeChunk :: CodeVarChunk -> CodeVarChunk
diffCodeChunk c = quantvar $ implVar' ("d" ++ c ^. uid) 
  (compoundPhrase (nounPhraseSP "change in") (c ^. term)) (getA c) (c ^. typ)
  (Concat [Label "d", symbol c Implementation]) (getUnit c) 

-- FIXME: This is surely a hack, but I can't think of a better way right now.
-- Some libraries use an array instead of a list to internally represent the ODE
-- So we need a way to switch the dependent variable from list to array,
-- and the array version must have a distinct UID so it can be stored in the DB
modifiedODESyst :: String -> ODEInfo -> [Expr]
modifiedODESyst sufx info = map replaceDepVar (odeSyst info)
  where replaceDepVar (C c) = if c == depVar info ^. uid 
          then C (c ++ "_" ++ sufx) else C c
        replaceDepVar (AssocA a es) = AssocA a (map replaceDepVar es)
        replaceDepVar (AssocB b es) = AssocB b (map replaceDepVar es)
        replaceDepVar (Deriv dt e u) = Deriv dt (replaceDepVar e) u
        replaceDepVar (FCall u es nes) = FCall u (map replaceDepVar es) 
          (map (over _2 replaceDepVar) nes)
        replaceDepVar (New u es nes) = New u (map replaceDepVar es) 
          (map (over _2 replaceDepVar) nes)
        replaceDepVar (Message au mu es nes) = Message au mu 
          (map replaceDepVar es) (map (over _2 replaceDepVar) nes)
        replaceDepVar (Case c cs) = Case c (map (over _1 replaceDepVar) cs)
        replaceDepVar (Matrix es) = Matrix $ map (map replaceDepVar) es
        replaceDepVar (UnaryOp u e) = UnaryOp u $ replaceDepVar e
        replaceDepVar (BinaryOp b e1 e2) = BinaryOp b (replaceDepVar e1) 
          (replaceDepVar e2)
        replaceDepVar (Operator ao dd e) = Operator ao dd $ replaceDepVar e 
        replaceDepVar (IsIn e s) = IsIn (replaceDepVar e) s
        replaceDepVar e = e