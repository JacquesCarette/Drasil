module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODE, scipyCall, oslo, osloCall, apacheODE, apacheODECall, odeint, 
  odeintCall, odeInfo, odeOptions
) where

import Language.Drasil

import Language.Drasil.Code (ODEMethod(..), ExternalLibrary, Step, Argument, 
  externalLib, mandatoryStep, mandatorySteps, choiceSteps, choiceStep,
  callStep, callRequiresJust, callRequires, libFunction, libMethod, 
  libFunctionWithResult, libMethodWithResult, libConstructor, 
  constructAndReturn, lockedArg, lockedNamedArg, inlineArg, inlineNamedArg, 
  preDefinedArg, functionArg, customObjArg, recordArg, lockedParam, 
  unnamedParam, customClass, implementation, constructorInfo, methodInfo, 
  methodInfoNoReturn, appendCurrSol, populateSolList, assignArrayIndex, 
  assignSolFromObj, initSolListFromArray, initSolListWithVal, 
  solveAndPopulateWhile, returnExprList, fixedReturn,
  ExternalLibraryCall, externalLibCall, choiceStepsFill, choiceStepFill, 
  mandatoryStepFill, mandatoryStepsFill, callStepFill, libCallFill, 
  basicArgFill, functionArgFill, customObjArgFill, recordArgFill, 
  unnamedParamFill, userDefinedParamFill, customClassFill, implementationFill, 
  constructorInfoFill, methodInfoFill, appendCurrSolFill, populateSolListFill, 
  assignArrayIndexFill, assignSolFromObjFill, initSolListFromArrayFill, 
  initSolListWithValFill, solveAndPopulateWhileFill, returnExprListFill, 
  fixedStatementFill, CodeVarChunk, CodeFuncChunk, quantvar, quantfunc, 
  ccObjVar)

import Control.Lens ((^.))

-- SciPy -- 

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ callRequiresJust scipyImport $ libFunctionWithResult 
    odefunc [
      functionArg f (map unnamedParam [Real, Array Real]) 
      returnExprList] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatorySteps [callStep $ libMethod r setInitVal 
      [inlineArg Real, inlineArg Real],
    initSolListWithVal,
    solveAndPopulateWhile (libMethod r successful []) rt 
      (libMethod r integrateStep [inlineArg Real]) ry]]

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
setIntegratorMethod = callStep . libMethod r setIntegrator

odeT :: Space
odeT = Actor "ode"

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

r, rt, ry :: CodeVarChunk
r = quantvar $ implVar "r_scipy" (nounPhrase "ODE object" "ODE objects") 
  odeT (Label "r")
rt = ccObjVar r t
ry = ccObjVar r y

f, odefunc, setIntegrator, setInitVal, successful, 
  integrateStep :: CodeFuncChunk
f = quantfunc $ implVar "f_scipy" (nounPhrase "function representing ODE system" 
  "functions representing ODE system") (Array Real) (Label "f")
odefunc = quantfunc $ implVar "ode_scipy" (nounPhrase 
  "function for defining an ODE for SciPy" 
  "functions for defining an ODE for SciPy") 
  odeT (Label (scipyImport ++ ".ode"))
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


-- Oslo (C#) --

oslo :: ExternalLibrary
oslo = externalLib [
  mandatoryStep $ callRequiresJust "Microsoft.Research.Oslo" $ libConstructor 
    vector [inlineArg Real] initv,
  choiceStep $ map (\s -> callStep $ libFunctionWithResult s odeArgs sol) 
    [rk547m, gearBDF],
  mandatorySteps (callRequiresJust "System.Linq" (libMethodWithResult sol 
      solveFromToStep (map inlineArg [Real, Real, Real]) points) :
    populateSolList points sp x)]

osloCall :: ODEInfo -> ExternalLibraryCall
osloCall info = externalLibCall [
  mandatoryStepFill $ callStepFill $ libCallFill [basicArgFill $ initVal info],
  choiceStepFill (chooseMethod $ solveMethod $ odeOpts info) $ callStepFill $ 
    libCallFill [basicArgFill $ tInit info, 
      functionArgFill (map unnamedParamFill [indepVar info, depVar info]) $ 
        returnExprListFill (odeSyst info), 
      recordArgFill [absTol $ odeOpts info, relTol $ odeOpts info]],
  mandatoryStepsFill [callStepFill $ libCallFill $ map basicArgFill 
      [tInit info, tFinal info, stepSize $ odeOpts info],
    populateSolListFill $ depVar info]]
  where chooseMethod RK45 = 0
        chooseMethod BDF = 1
        chooseMethod _ = error odeMethodUnavailable

odeArgs :: [Argument]
odeArgs = [inlineArg Real, lockedArg (sy initv),
  functionArg fOslo (map unnamedParam [Real, vecT]) 
    (callStep $ constructAndReturn vector [inlineArg Real]),
  recordArg options opts [aTol, rTol]]

solT, vecT, optT :: Space
solT = Actor "IEnumerable<SolPoint>"
vecT = Actor "Vector"
optT = Actor "Options"

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

-- Apache (Java) --

apacheODE :: ExternalLibrary
apacheODE = externalLib [
  choiceStep [
    callRequires [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ adams]
      $ libConstructor adamsC (lockedArg (int 3) : itArgs) it,
    callRequires [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ dp54]
      $ libConstructor dp54C itArgs it],
  mandatorySteps [callStep $ libMethod it addStepHandler [
      customObjArg (map ((apacheImport ++ "sampling.") ++) [sh, si]) 
        "Class defining additional behaviour for each step of an ODE solution"
        stepHandler stepHandlerCtor (implementation sh [
          methodInfoNoReturn initMethod 
            "initializes step handler with initial conditions" 
            (map lockedParam [t0, y0, t]) [initSolListFromArray y0],
          methodInfoNoReturn handleStep 
            "appends solution point at each ODE solution step"
            (map lockedParam [interpolator, isLast]) 
            [callStep $ libMethodWithResult interpolator getInterpState [] curr,
            appendCurrSol curr]])],
    callStep $ libMethod it integrate (customObjArg [apacheImport ++ fode] 
      "Class representing an ODE system" ode odeCtor (implementation fode [
        constructorInfo odeCtor [] [],
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
      customObjArgFill [depVar info] (implementationFill [
        methodInfoFill [] [initSolListFromArrayFill $ depVar info], methodInfoFill [] 
          [callStepFill $ libCallFill [], appendCurrSolFill $ depVar info]])],
    callStepFill $ libCallFill $ customObjArgFill (otherVars info) 
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

apacheImport :: String
apacheImport = "org.apache.commons.math3.ode."

itArgs :: [Argument]
itArgs = map inlineArg [Real, Real, Real, Real]

adams, dp54, foi, sh, si, fode :: String
adams = "AdamsBashforthIntegrator"
dp54 = "DormandPrince54Integrator"
foi = "FirstOrderIntegrator"
sh = "StepHandler"
si = "StepInterpolator"
fode = "FirstOrderDifferentialEquations"

it, currVals, stepHandler, t0, y0, interpolator, isLast, curr :: CodeVarChunk
it = quantvar $ implVar "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") (Actor foi) (Label "it")
currVals = quantvar $ implVar "curr_vals_apache" (nounPhrase 
  "array holding ODE solution values for the current step"
  "arrays holding ODE solution values for the current step") 
  (Array Real) (Label "curr_vals")
stepHandler = quantvar $ implVar "stepHandler_apache" (nounPhrase 
  "ODE step handler" "ODE step handlers") (Actor sh)
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
  (Actor sh) (Label "StepHandler")
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

odeint :: ExternalLibrary
odeint = externalLib [
  choiceSteps [
    [callStep $ libConstructor rkdp5C [] rk,
    callStep $ libFunctionWithResult makeControlled 
      [inlineArg Real, inlineArg Real, lockedArg (sy rk)] stepper],
    [callStep $ libConstructor adamsBashC [] stepper]],
  mandatoryStep $ callRequiresJust "boost/numeric/odeint.hpp" $ libFunction 
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
    customObjArgFill (otherVars info) (customClassFill [
      constructorInfoFill (map userDefinedParamFill $ otherVars info) 
        (zip (otherVars info) (map sy $ otherVars info)) [], 
      methodInfoFill (map unnamedParamFill [depVar info, ddep]) 
        [assignArrayIndexFill ddep (odeSyst info)]]) :
    map basicArgFill [Matrix [[initVal info]], tInit info, tFinal info, 
      stepSize $ odeOpts info] ++ [
    customObjArgFill [depVar info] (customClassFill [
      constructorInfoFill [userDefinedParamFill $ depVar info] 
        [(depVar info, sy $ depVar info)] [],
      methodInfoFill [] [appendCurrSolFill $ depVar info]])]]
  where chooseMethod RK45 = (0, map (callStepFill . libCallFill . map 
          basicArgFill) [[], [absTol $ odeOpts info, relTol $ odeOpts info]])
        chooseMethod Adams = (1, [callStepFill $ libCallFill []])
        chooseMethod _ = error odeMethodUnavailable
        ddep = diffCodeChunk $ depVar info

odeNameSpace, rkdp5, adamsBash :: String
odeNameSpace = "boost::numeric::odeint::"
rkdp5 = odeNameSpace ++ "runge_kutta_dopri5<vector<double>>"
adamsBash = odeNameSpace ++ "adams_bashforth<3,vector<double>>"

popT :: Space
popT = Actor "Populate"

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

-- Data 

-- This may be temporary, but need a structure to hold ODE info for now. 
-- Goal will be for this info to be populated by the instance model for the ODE and the Choices structure.
-- Probably doesn't belong here, but where?
data ODEInfo = ODEInfo {
  indepVar :: CodeVarChunk,
  depVar :: CodeVarChunk,
  otherVars :: [CodeVarChunk],
  tInit :: Expr,
  tFinal :: Expr,
  initVal :: Expr,
  odeSyst :: [Expr],
  odeOpts :: ODEOptions
}

odeInfo :: CodeVarChunk -> CodeVarChunk -> [CodeVarChunk] -> Expr -> Expr -> 
  Expr -> [Expr] -> ODEOptions -> ODEInfo
odeInfo = ODEInfo

data ODEOptions = ODEOpts {
  solveMethod :: ODEMethod,
  absTol :: Expr,
  relTol :: Expr,
  stepSize :: Expr
}

odeOptions :: ODEMethod -> Expr -> Expr -> Expr -> ODEOptions
odeOptions = ODEOpts

diffCodeChunk :: CodeVarChunk -> CodeVarChunk
diffCodeChunk c = quantvar $ implVar' ("d" ++ c ^. uid) 
  (compoundPhrase (nounPhraseSP "change in") (c ^. term)) (getA c) (c ^. typ)
  (Concat [Label "d", symbol c Implementation]) (getUnit c) 
