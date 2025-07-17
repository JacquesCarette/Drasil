-- | Define and collect information about ODEs and ODE solvers from various libraries.
module Data.Drasil.ExternalLibraries.ODELibraries (
  -- * SciPy Library (Python)
  scipyODEPckg, scipyODESymbols,
  -- * Oslo Library (C#)
  osloPckg, osloSymbols, arrayVecDepVar,
  -- * Apache Commons (Java)
  apacheODEPckg, apacheODESymbols,
  -- * Odeint (C++)
  odeintPckg, odeintSymbols
) where

import Language.Drasil (HasSymbol(symbol), HasUID(uid), MayHaveUnit(getUnit),
  QuantityDict, HasSpace(typ), Space (Actor, Natural, Real, Void, Boolean, String, Array, Vect), dqdNoUnit,
  qw, compoundPhrase, nounPhrase, nounPhraseSP, label,
  sub, Idea(getA), NamedIdea(term), Stage(..), (+++), dcc, dccAWDS, dqd', Definition (defn), (+:+), Sentence (S), dccWDS)
import Language.Drasil.Display (Symbol(Label, Concat))

import Language.Drasil.Code (Lang(..), ExternalLibrary, Step, Argument,
  externalLib, mandatoryStep, mandatorySteps, choiceSteps, choiceStep,
  callStep, libFunction, libMethod, libFunctionWithResult, libMethodWithResult,
  libConstructor, libConstructorMultiReqs, constructAndReturn, lockedArg,
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, functionArg,
  customObjArg, recordArg, lockedParam, unnamedParam, customClass,
  implementation, constructorInfo, methodInfo, methodInfoNoReturn,
  appendCurrSol, populateSolList, assignArrayIndex, assignSolFromObj,
  initSolListFromArray, initSolListWithVal, solveAndPopulateWhile,
  returnExprList, fixedReturn',
  ExternalLibraryCall, externalLibCall, choiceStepsFill, choiceStepFill,
  mandatoryStepFill, mandatoryStepsFill, callStepFill, libCallFill,
  userDefinedArgFill, basicArgFill, functionArgFill, customObjArgFill,
  recordArgFill, unnamedParamFill, unnamedParamPBVFill, userDefinedParamFill,
  customClassFill, implementationFill, constructorInfoFill, methodInfoFill,
  appendCurrSolFill, populateSolListFill, assignArrayIndexFill,
  assignSolFromObjFill, initSolListFromArrayFill, initSolListWithValFill,
  solveAndPopulateWhileFill, returnExprListFill, fixedStatementFill',
  CodeVarChunk, CodeFuncChunk, quantvar, quantfunc, listToArray,
  ODEInfo(..), ODEOptions(..), ODEMethod(..), ODELibPckg, mkODELib,
  mkODELibNoPath, pubStateVar, privStateVar,
  NamedArgument, narg)

import Drasil.Code.CodeExpr
import Drasil.Code.CodeExpr.Development

import Control.Lens ((^.), _1, _2, over)

-- SciPy Library (Python)

-- | [SciPy](https://www.scipy.org/) ODE library package.
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
      [matrix [initVal info], tInit info],
    initSolListWithValFill (depVar info) (matrix [initVal info]),
    solveAndPopulateWhileFill (libCallFill []) (tFinal info)
    (libCallFill [basicArgFill (field r t $+ stepSize (odeOpts info))])
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

odeT, numpyArrayT :: Space
odeT = Actor "ode"
numpyArrayT = Actor "numpyArray"

-- | Collects variables needed for SciPy's ODEs as 'QuantityDict's.
scipyODESymbols :: [QuantityDict]
scipyODESymbols = map qw [mthdArg, atolArg, rtolArg]
  ++ map qw [r, t, y, xAxis, ut, transpose]
  ++ map qw [f, odefunc, setIntegrator, setInitVal, successful, integrateStep,
  arange, odeintFunc]

mthdArg, atolArg, rtolArg :: NamedArgument
mthdArg = narg $ dqdNoUnit (dcc "method_scipy" (nounPhrase
  "chosen method for solving ODE" "chosen methods for solving ODE")
  "the chosen method for solving the ODE")
  (label "method") String
atolArg = narg $ dqdNoUnit (dcc "atol_scipy" (nounPhrase
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution")
  "the absolute tolerance for the ODE solution")
  (label "atol") Real
rtolArg = narg $ dqdNoUnit (dcc "rtol_scipy" (nounPhrase
  "relative tolerance for ODE solution" "relative tolerances for ODE solution")
  "the relative tolerance for the ODE solution")
  (label "rtol") Real


r, xAxis, ut, transpose :: CodeVarChunk
r = quantvar $ dqdNoUnit (dcc "r_scipy" (nounPhrase "ODE object" "ODE objects")
  "the ODE object")
  (label "r") odeT
xAxis = quantvar $ dqdNoUnit (dcc "x_numpy" (nounPhrase "Numpy value" "Numpy value")
  "the x-axis Numpy value")
  (label "x_axis") (Array Real)
ut = quantvar $ dqdNoUnit (dcc "ut_scipy"
  (nounPhrase "Scipy integrated value" "Scipy integrated value")
  "the Scipy integrated value")
  (label "u_t") numpyArrayT
transpose = quantvar $ dqdNoUnit (dcc "transpose_numpy"
  (nounPhrase "Numpy Array Transpose" "Numpy Array Transpose")
  "the Numpy Array Transpose")
  (label "u_t.T") (Array Real) -- (ccObjVar ut transpose) does not seem to work.


f, odefunc, setIntegrator, setInitVal, successful,
  integrateStep, arange, odeintFunc :: CodeFuncChunk
f = quantfunc $ dqdNoUnit (dcc "f_scipy" (nounPhrase "function representing ODE system"
  "functions representing ODE system") "the function representing the ODE system")
  (label "f") (Array Real)
odefunc = quantfunc $ dqdNoUnit (dcc "ode_scipy" (nounPhrase
  "function for defining an ODE for SciPy"
  "functions for defining an ODE for SciPy")
  "the function for defining an ODE for SciPy") (label "ode") odeT
setIntegrator = quantfunc $ dqdNoUnit (dcc "set_integrator_scipy" (nounPhrase
  "method for setting SciPy integrator" "methods for setting SciPy integrator")
  "the method for setting the SciPy integrator") (label "set_integrator") Void
setInitVal = quantfunc $ dqdNoUnit (dcc "set_initial_value_scipy" (nounPhrase
  "method for setting initial value for ODE for SciPy"
  "methods for setting initial value for ODE for SciPy")
  "the method for setting the initial value for the ODE for SciPy")
  (label "set_initial_value") Void
successful = quantfunc $ dqdNoUnit (dcc "successful_scipy" (nounPhrase
  "method returning True if integration is current successful"
  "methods returning True if integration is current successful")
  "the method returning True if integration is currently successful")
  (label "successful") Boolean
integrateStep = quantfunc $ dqdNoUnit (dcc "integrate_scipy" (nounPhrase
  "method that performs one integration step on an ODE"
  "methods that perform one integration step on an ODE")
  "the method that performs one integration step on an ODE")
  (label "integrate") Void
arange = quantfunc $ dqdNoUnit (dcc "arrange_numpy" (nounPhrase
  "method that returns evenly spaced numbers over a specified interval."
  "method that returns evenly spaced numbers over a specified interval.")
  "the method that returns evenly spaced numbers over a specified interval.")
  (label "arange") (Array Real)
odeintFunc = quantfunc $ dqdNoUnit (dcc "odeint_scipy" (nounPhrase
  "method that solves a system of ODE using lsoda from the FORTRAN library odepack."
  "method that solves a system of ODE using lsoda from the FORTRAN library odepack.")
  "the method that solves a system of ODEs using lsoda from the FORTRAN library odepack.")
  (label "odeint") (Array Real)

-- Oslo Library (C#)

-- | [Oslo](https://www.microsoft.com/en-us/research/project/open-solving-library-for-odes/) ODE library package.
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
  mandatoryStepFill $ callStepFill $ libCallFill [basicArgFill $ matrix [initVal info]],
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

-- | Collects variables needed for Oslo's ODEs as 'QuantityDict's.
osloSymbols :: [QuantityDict]
osloSymbols = map qw [initv, opts, aTol, rTol, sol, points, sp, x] ++
  map qw [fOslo, options, vector, rk547m, gearBDF, solveFromToStep]

initv, opts, aTol, rTol, sol, points, sp, x :: CodeVarChunk
initv = quantvar $ dqdNoUnit (dcc "initv_oslo" (nounPhrase
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables")
  "the vector containing the initial values of the dependent variables")
  (label "initv") vecT
opts = quantvar $ dqdNoUnit (dcc "opts_oslo" (nounPhrase
  "record containing options for ODE solving"
  "records containing options for ODE solving")
  "the record containing options for ODE solving")
  (label "opts") optT
aTol = quantvar $ dqdNoUnit (dcc "aTol_oslo" (nounPhrase
  "absolute tolerance for ODE solution" "absolute tolerances for ODE solution")
  "the absolute tolerance for the ODE solution")
  (label "AbsoluteTolerance") Real
rTol = quantvar $ dqdNoUnit (dcc "rTol_oslo" (nounPhrase
  "relative tolerance for ODE solution" "relative tolerances for ODE solution")
  "the relative tolerance for the ODE solution")
  (label "RelativeTolerance") Real
sol = quantvar $ dqdNoUnit (dcc "sol_oslo" (nounPhrase 
  "container for ODE information" "containers for ODE information") 
  "the container for ODE information") (label "sol") solT
points = quantvar $ dqdNoUnit (dcc "points_oslo" (nounPhrase
  "container holding ODE solution" "containers holding ODE solution")
  "the container holding the ODE solution")
  (label "points") solT
sp = quantvar $ dqdNoUnit (dcc "sp_oslo" (nounPhrase "ODE solution point"
  "ODE solution points") "the ODE solution point") 
  (label "sp") (Actor "SolPoint")
x = quantvar $ dqdNoUnit (dcc "X_oslo" (nounPhrase "dependent variable"
  "dependent variables") "the dependent variable")
  (label "X") (Array Real)

fOslo, options, vector, rk547m, gearBDF, solveFromToStep :: CodeFuncChunk
fOslo = quantfunc $ dqdNoUnit (dcc "f_oslo" (nounPhrase
  "function representing ODE system" "functions representing ODE system")
  "the function representing the ODE system")
  (label "f") vecT
options = quantfunc $ dqdNoUnit (dcc "Options_oslo" (nounPhrase
  "constructor for Options record" "constructors for Options record")
  "the constructor for the Options record")
  (label "Options") optT
vector = quantfunc $ dqdNoUnit (dcc "Vector_oslo" (nounPhrase
  "constructor for an OSLO Vector" "constructors for an OSLO Vector")
  "the constructor for an OSLO Vector")
  (label "Vector") vecT
rk547m = quantfunc $ dqdNoUnit (dcc "RK547M_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Runge-Kutta method"
  "functions for initiating an ODE to be solved by Runge-Kutta method")
  "the function for initiating an ODE to be solved by the Runge-Kutta method")
  (label "Ode.RK547M") solT
gearBDF = quantfunc $ dqdNoUnit (dcc "GearBDF_oslo" (nounPhrase
  "function for initiating an ODE to be solved by Gear's BDF method"
  "functions for initiating an ODE to be solved by Gear's BDF method")
  "the function for initiating an ODE to be solved by Gear's BDF method")
  (label "Ode.GearBDF") solT
solveFromToStep = quantfunc $ dqdNoUnit (dcc "SolveFromToStep_oslo" (nounPhrase
  "method for solving an ODE given a time range"
  "methods for solving an ODE given a time range")
  "the method for solving an ODE given a time range")
  (label "SolveFromToStep") solT

vecDepVar :: ODEInfo -> CodeVarChunk
vecDepVar info = quantvar $ dqdNoUnit (dccWDS (show $ dv ^. uid) (dv ^. term)
  (dv ^. defn)) (sub (symbol dv Implementation) (label "vec")) vecT
  where dv = depVar info

-- Hack required because
-- | Oslo's Vector type behaves like an array, so needs to
-- be represented as one or else will hit type errors in GOOL.
arrayVecDepVar :: ODEInfo -> CodeVarChunk
arrayVecDepVar info = quantvar $ dqdNoUnit (dccWDS (show $ dv +++ "vec")
  (dv ^. term) (dv ^. defn)) (sub (symbol dv Implementation) (label "vec"))
  (dv ^. typ)
  where dv = listToArray $ depVar info

-- Apache Commons (Java)

-- | [Apache Commons](https://commons.apache.org/) ODE library package.
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
          [] "dimension of the ODE system" [fixedReturn'],
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
    libCallFill (map (basicArgFill . ($ odeOpts info)) [stepSize, stepSize, absTol, relTol]),
  mandatoryStepsFill [callStepFill $ libCallFill [
      customObjArgFill [pubStateVar $ depVar info] (implementationFill [
        methodInfoFill [] [initSolListFromArrayFill $ depVar info], methodInfoFill []
          [callStepFill $ libCallFill [], appendCurrSolFill $ depVar info]])],
    callStepFill $ libCallFill $ customObjArgFill
      (map privStateVar $ otherVars info)
      (implementationFill [
        constructorInfoFill (map userDefinedParamFill $ otherVars info)
          (zip (otherVars info) (map sy $ otherVars info)) [],
        methodInfoFill [] [fixedStatementFill' $ int $ toInteger $ length $ initVal info],
        methodInfoFill (map (unnamedParamFill . listToArray) [depVar info, ddep])
          [assignArrayIndexFill (listToArray ddep) (modifiedODESyst "array" info)]])
      : map basicArgFill [tInit info, matrix [initVal info], tFinal info,
        matrix [initVal info]],
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

-- | Collects variables needed for Apache's ODEs as 'QuantityDict's.
apacheODESymbols :: [QuantityDict]
apacheODESymbols = map qw [it, currVals, stepHandler, t0, y0, interpolator,
  isLast, curr, ode] ++ map qw [adamsC, dp54C, stepHandlerCtor, addStepHandler,
  initMethod, handleStep, getInterpState, integrate, odeCtor, getDimension,
  computeDerivatives]

it, currVals, stepHandler, t0, y0, interpolator, isLast, curr :: CodeVarChunk
it = quantvar $ dqdNoUnit (dcc "it_apache" (nounPhrase "integrator for solving ODEs"
  "integrators for solving ODEs") "the integrator for solving ODEs")
  (label "it") (Actor foi)
currVals = quantvar $ dqdNoUnit (dcc "curr_vals_apache" (nounPhrase
  "array holding ODE solution values for the current step"
  "arrays holding ODE solution values for the current step")
  "the array holding ODE solution values for the current step")
  (label "curr_vals") (Array Real)
stepHandler = quantvar $ dqdNoUnit (dcc "stepHandler_apache" (nounPhrase
  "ODE step handler" "ODE step handlers") "the ODE step handler") 
  (label "stepHandler") (Actor $ "ODE" ++ sh)
t0 = quantvar $ dqdNoUnit (dcc "t0_apache" (nounPhrase "initial time for ODE solving"
  "intial times for ODE solving") "the initial time for ODE solving")
   (label "t0") Real
y0 = quantvar $ dqdNoUnit (dcc "y0_apache" (nounPhrase
  "array of initial values for ODE solving"
  "arrays of initial values for ODE solving")
  "the array of initial values for ODE solving") (label "y0") (Array Real)
interpolator = quantvar $ dqdNoUnit (dcc "interpolator_apache" (nounPhrase
  "step interpolator for ODE solving" "step interpolator for ODE solving")
  "the step interpolator for ODE solving") (label "interpolator") (Actor si)
isLast = quantvar $ dqdNoUnit (dcc "isLast_apache" (nounPhrase
  "boolean for whether the current step is the last step"
  "booleans for whether the current step is the last step")
  "the boolean for whether the current step is the last step")
  (label "isLast") Boolean
curr = quantvar $ dqdNoUnit (dcc "curr_apache" (nounPhrase
  "ODE solution array for current step" "ODE solution arrays for current step")
  "the ODE solution array for the current step")
  (label "curr") (Array Real)

adamsC, dp54C, stepHandlerCtor, addStepHandler, initMethod, handleStep,
  getInterpState, integrate, getDimension, computeDerivatives :: CodeFuncChunk
adamsC = quantfunc $ dqdNoUnit (dcc "adams_ctor_apache" (nounPhrase
  "constructor for an Adams-Bashforth integrator"
  "constructors for an Adams-Bashforth integrator") 
  "the constructors for an Adams-Bashforth integrator")
  (Label adams) (Actor adams)
dp54C = quantfunc $ dqdNoUnit (dcc "dp54_ctor_apache" (nounPhrase
  "constructor for a Dormand-Prince 5-4 integrator"
  "constructors for a Dormand-Prince 5-4 integrator")
  "the constructors for a Dormand-Prince 5-4 integrator")
  (Label dp54) (Actor dp54)
stepHandlerCtor = quantfunc $ dqdNoUnit (dcc "StepHandler_ctor_apache" (nounPhrase
  "constructor for StepHandler" "constructors for StepHandler")
  "the constructor for StepHandler")
  (Label $ "ODE" ++ sh) (Actor $ "ODE" ++ sh)
addStepHandler = quantfunc $ dqdNoUnit (dcc "addStepHandler_apache" (nounPhrase
  "method for adding a step handler to an integrator"
  "methods for adding a step handler to an integrator")
  "the method for adding a step handler to an integrator")
  (label "addStepHandler") Void
initMethod = quantfunc $ dqdNoUnit (dcc "init_apache" (nounPhrase
  "method to initialize step handler" "methods to initialize step handler")
  "the method to initialize the step handler") (label "init") Void
handleStep = quantfunc $ dqdNoUnit (dcc "handleStep_apache" (nounPhrase
  "method to call at each ODE step" "methods to call at each ODE step")
  "the method to call at each ODE step") (label "handleStep") Void
getInterpState = quantfunc $ dqdNoUnit (dcc "getInterpolatedState_apache" (nounPhrase
  "method for getting current state during ODE solving"
  "methods for getting current state during ODE solving")
  "the method for getting the current state during ODE solving")
  (label "getInterpolatedState") (Array Real)
integrate = quantfunc $ dqdNoUnit (dcc "integrate_apache" (nounPhrase
  "method for integrating an ODE" "methods for integrating an ODE")
  "the method for integrating an ODE") (label "integrate") Void
getDimension = quantfunc $ dqdNoUnit (dcc "getDimension_apache" (nounPhrase
  "method returning the dimension of an ODE system"
  "methods returning the dimension of an ODE system")
  "the method returning the dimension of an ODE system")
  (label "getDimension") Natural
computeDerivatives = quantfunc $ dqdNoUnit (dcc "computeDerivatives_apache" (nounPhrase
  "method encoding an ODE system" "methods encoding an ODE system")
  "the method encoding an ODE system") (label "computeDerivatives") Void

-- odeint (C++)

-- | [odeint](https://headmyshoulder.github.io/odeint-v2/) ODE library package.
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
    map basicArgFill [matrix [initVal info], tInit info, tFinal info,
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

-- | Collects variables needed for odeint's ODEs as 'QuantityDict's.
odeintSymbols :: [QuantityDict]
odeintSymbols = map qw [odeintCurrVals, rk, stepper, pop] ++ map qw
  [rkdp5C, makeControlled, adamsBashC, integrateConst, odeOp, popCtor,
  popOp]

odeintCurrVals, rk, stepper, pop :: CodeVarChunk
odeintCurrVals = quantvar $ dqdNoUnit (dcc "currVals_odeint" (nounPhrase
  "vector holding ODE solution values for the current step"
  "vectors holding ODE solution values for the current step")
  "the vector holding the ODE solution values for the current step")
  (label "currVals") (Vect Real)
rk = quantvar $ dqdNoUnit (dcc "rk_odeint" (nounPhrase
  "stepper for solving ODE system using Runge-Kutta-Dopri5 method"
  "steppers for solving ODE system using Runge-Kutta-Dopri5 method")
  "the stepper for solving the ODE system using the Runge-Kutta-Dopri5 method")
  (label "rk") (Actor rkdp5)
stepper = quantvar $ dqdNoUnit (dcc "stepper_odeint" (nounPhrase
  "stepper for solving ODE system" "steppers for solving ODE system")
  "the stepper for solving the ODE system") (label "stepper") (Actor "auto")
pop = quantvar $ dqdNoUnit (dcc "pop_odeint" (nounPhrase
  "object to populate ODE solution vector"
  "objects to populate ODE solution vector")
  "the object to populate the ODE solution vector") (label "pop") popT

rkdp5C, makeControlled, adamsBashC, integrateConst, odeOp, popCtor,
  popOp :: CodeFuncChunk
rkdp5C = quantfunc $ dqdNoUnit (dcc "rkdp5_odeint" (nounPhrase
  "constructor for stepper using Runge-Kutta-Dopri5 method"
  "constructors for stepper using Runge-Kutta-Dopri5 method")
  "the constructor for stepper using the Runge-Kutta-Dopri5 method")
  (Label rkdp5) (Actor rkdp5)
makeControlled = quantfunc $ dqdNoUnit (dcc "make_controlled_odeint" (nounPhrase
  "function for adding error control to a stepper"
  "functions for adding error control to a stepper")
  "the function for adding error control to a stepper")
  (Label $ odeNameSpace ++ "make_controlled") (Actor "auto")
adamsBashC = quantfunc $ dqdNoUnit (dcc "adamsBash_odeint" (nounPhrase
  "constructor for stepper using Adams-Bashforth method"
  "constructors for stepper using Adams-Bashforth method")
  "the constructor for stepper using the Adams-Bashforth method")
  (Label adamsBash) (Actor adamsBash)
integrateConst = quantfunc $ dqdNoUnit (dcc "integrate_const_odeint" (nounPhrase
  "function for integrating with a constant step size"
  "functions for integrating with a constant step size")
  "the function for integrating with a constant step size")
  (Label $ odeNameSpace ++ "integrate_const") Void
odeOp = quantfunc $ dqdNoUnit (dcc "ode_operator_odeint" (nounPhrase
  "method defining override for calling ODE object"
  "methods defining override for calling ODE object")
  "the method defining override for calling an ODE object")
  (label "operator()") Void
popCtor = quantfunc $ dqdNoUnit (dcc "Populate_odeint" (nounPhrase
  "constructor for Populate object for ODE solving with odeint"
  "constructors for Populate object for ODE solving with odeint")
  "the constructor for the Populate object for ODE solving with odeint")
  (label "Populate") popT
popOp = quantfunc $ dqdNoUnit (dcc "pop_operator_odeint" (nounPhrase
  "method defining override for calling Populate object"
  "methods defining override for calling Populate object")
  "the method defining override for calling a Populate object")
  (label "operator()") Void

-- 'CodeChunk's used in multiple external ODE libraries

ode, t, y :: CodeVarChunk
-- | ODE object & definition.
ode = quantvar $ dqdNoUnit (dcc "ode_obj" (nounPhrase
  "object representing an ODE system" "objects representing an ODE system")
  "the object representing an ODE system") (label "ode") odeObj
-- | Independent variable in an ODE.
t = quantvar $ dqdNoUnit (dcc "t_ode" (nounPhrase
  "current independent variable value in ODE solution"
  "current independent variable value in ODE solution")
  "the current independent variable value in the ODE solution")
  (label "t") Real
-- | Dependent variable in an ODE.
y = quantvar $ dqdNoUnit (dcc "y_ode" (nounPhrase
  "current dependent variable value in ODE solution"
  "current dependent variable value in ODE solution")
  "the current dependent variable value in the ODE solution")
  (label "y") (Vect Real)

-- | ODE object constructor.
odeCtor :: CodeFuncChunk
odeCtor = quantfunc $ dqdNoUnit (dcc "ODE_constructor" (nounPhrase
  "constructor for ODE object" "constructors for ODE object")
  "the constructor for the ODE object") (label "ODE") odeObj

-- | ODE object.
odeObj :: Space
odeObj = Actor "ODE"

-- | ODE method unavailable message.
odeMethodUnavailable :: String
odeMethodUnavailable = "Chosen ODE solving method is not available" ++
          " in chosen ODE solving library"

-- | Change in @X@ chunk constructor (where @X@ is a given argument).
diffCodeChunk :: CodeVarChunk -> CodeVarChunk
diffCodeChunk c = quantvar $ dqd' (dccAWDS (show $ c +++ "d" )
  (compoundPhrase (nounPhraseSP "change in") (c ^. term)) 
  (S "the change in" +:+ (c ^. defn)) (getA c)) 
  (const (Concat [label "d", symbol c Implementation])) (c ^. typ) (getUnit c)

-- FIXME: This is surely a hack, but I can't think of a better way right now.
-- | Some libraries use an array instead of a list to internally represent the ODE.
-- So we need a way to switch the dependent variable from list to array,
-- and the array version must have a distinct UID so it can be stored in the DB.
modifiedODESyst :: String -> ODEInfo -> [CodeExpr]
modifiedODESyst sufx info = map replaceDepVar (odeSyst info)
  where
    replaceDepVar cc@(C c) | c == depVar info ^. uid = C $ depVar info +++ ("_" ++ sufx)
                           | otherwise               = cc
    replaceDepVar (AssocA a es)           = AssocA a (map replaceDepVar es)
    replaceDepVar (AssocB b es)           = AssocB b (map replaceDepVar es)
    replaceDepVar (FCall u es nes)        = FCall u (map replaceDepVar es)
      (map (over _2 replaceDepVar) nes)
    replaceDepVar (New u es nes)          = New u (map replaceDepVar es)
      (map (over _2 replaceDepVar) nes)
    replaceDepVar (Message au mu es nes)  = Message au mu (map replaceDepVar es)
      (map (over _2 replaceDepVar) nes)
    replaceDepVar (Case c cs)             = Case c (map (over _1 replaceDepVar) cs)
    replaceDepVar (Matrix es)             = Matrix $ map (map replaceDepVar) es
    replaceDepVar (UnaryOp u e)           = UnaryOp u $ replaceDepVar e
    replaceDepVar (UnaryOpB u e)          = UnaryOpB u $ replaceDepVar e
    replaceDepVar (UnaryOpVV u e)         = UnaryOpVV u $ replaceDepVar e
    replaceDepVar (UnaryOpVN u e)         = UnaryOpVN u $ replaceDepVar e
    replaceDepVar (ArithBinaryOp b e1 e2) = ArithBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (BoolBinaryOp b e1 e2)  = BoolBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (EqBinaryOp b e1 e2)    = EqBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (LABinaryOp b e1 e2)    = LABinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (OrdBinaryOp b e1 e2)   = OrdBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (VVNBinaryOp b e1 e2)   = VVNBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (VVVBinaryOp b e1 e2)   = VVVBinaryOp b
      (replaceDepVar e1) (replaceDepVar e2)
    replaceDepVar (Operator ao dd e)      = Operator ao dd $ replaceDepVar e
    replaceDepVar e = e
