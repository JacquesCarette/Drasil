module Data.Drasil.ExternalLibraries.ODELibraries (
  scipyODE, oslo, apacheODE, odeint
) where

import Language.Drasil

import Language.Drasil.Code (FuncStmt(..), ExternalLibrary, Step, Argument, 
  externalLib, mandatoryStep, choiceSteps, choiceStep, callStep, 
  callWithImport, callWithImports, loopStep, libFunction, libMethod, 
  libFunctionWithResult, libMethodWithResult, libConstructor, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, preDefinedArg, functionArg, 
  customObjArg, recordArg, lockedParam, unnamedParam, customClass, 
  implementation, constructorInfo, methodInfo, appendCurrSol, populateSolList, 
  statementStep, fixedReturn, 
  CodeChunk, codevar, ccObjVar, implCQD)

import GOOL.Drasil (CodeType(Float, List, Array, Object, Func, Void))
import qualified GOOL.Drasil as C (CodeType(Boolean, Integer))

-- SciPy -- 

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ callWithImport scipyImport $ libFunctionWithResult 
    (scipyImport ++ ".ode") [
      functionArg f (map unnamedParam [Float, Array Float]) 
      (\es -> FRet $ Matrix [es])] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatoryStep $ callStep $ libMethod r "set_initial_value" [inlineArg Float],
  mandatoryStep $ statementStep (\cdch e -> FAsg (head cdch) (Matrix [[head e]])),
  mandatoryStep $ loopStep [libMethod r "successful" []] 
    (\cdch -> sy rt $< sy (head cdch)) 
    [callStep $ libMethod r "integrate" [inlineArg Float],
    appendCurrSol ry]] 

scipyImport :: String
scipyImport = "scipy.integrate"

atol, rtol, vode :: Argument
vode = lockedArg (str "vode")
atol = inlineNamedArg "atol" Float
rtol = inlineNamedArg "rtol" Float

methodArg :: String -> Argument
methodArg = lockedNamedArg "method" . str

setIntegratorMethod :: [Argument] -> Step
setIntegratorMethod = callStep . libMethod r "set_integrator"

odeT :: CodeType
odeT = Object "ode"

f, r, rt, ry :: CodeChunk
f = codevar $ implCQD "f_scipy" (nounPhrase "function representing ODE system" 
  "functions representing ODE system") Nothing 
  (Func [Float, Float] (List Float)) (Label "f") Nothing
r = codevar $ implCQD "r_scipy" (nounPhrase "ODE object" "ODE objects") Nothing 
  odeT (Label "r") Nothing
rt = ccObjVar r t
ry = ccObjVar r y

-- Oslo (C#) --

oslo :: ExternalLibrary
oslo = externalLib [
  mandatoryStep $ callWithImport "Microsoft.Research.Oslo" $ libConstructor 
    "Vector" [inlineArg Float] initv,
  choiceStep $ map (\s -> callStep $ libFunctionWithResult s odeArgs sol) 
    ["Ode.RK547M", "Ode.GearBDF"],
  mandatoryStep $ callWithImport "System.Linq" $ libMethodWithResult sol 
    "SolveFromToStep" (map inlineArg [Float, Float, Float]) points,
  mandatoryStep $ callStep $ libMethodWithResult points "ToArray" [] ptArray,
  mandatoryStep $ statementStep (\cdch _ -> FAsg (head cdch) (Matrix [[]])),
  mandatoryStep $ populateSolList ptArray sp x]

odeArgs :: [Argument]
odeArgs = [inlineArg Float, lockedArg (sy initv),
  -- Using Matrix here is an incorrect hack to make things compile for now
  -- The type I really need is Object "Vector", but Expr does not have a way of representing a constructor call, and shouldn't since constructor calls are very code-specific and Expr should not be code-specific
  -- Probably what I need to do is define a new type, CodeExpr, which extends Expr with some code-specific representations. Then both ExternalLibrary and FuncStmt would take arguments of type CodeExpr where they currently take Expr.
  functionArg fOslo (map unnamedParam [Float, Object "Vector"]) 
    (\es -> FRet $ Matrix [es]),
  recordArg "Options" opts ["AbsoluteTolerance", "RelativeTolerance"]]

solT :: CodeType
solT = Object "IEnumerable<SolPoint>"

initv, fOslo, opts, sol, points, ptArray, sp, x :: CodeChunk
initv = codevar $ implCQD "initv_oslo" (nounPhrase 
  "vector containing the initial values of the dependent variables"
  "vectors containing the initial values of the dependent variables") Nothing
  (Object "Vector") (Label "initv") Nothing
fOslo = codevar $ implCQD "f_oslo" (nounPhrase 
  "function representing ODE system" "functions representing ODE system") 
  Nothing (Func [Float, Object "Vector"] (Object "Vector")) (Label "f") Nothing
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
x = codevar $ implCQD "X_oslo" (nounPhrase "dependent variable" 
  "dependent variables") Nothing (Array Float) (Label "X") Nothing

-- Apache (Java) --

apacheODE :: ExternalLibrary
apacheODE = externalLib [
  choiceStep [
    callWithImports [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ adams]
      $ libConstructor adams (lockedArg (int 3) : itArgs) it,
    callWithImports [apacheImport ++ foi, apacheImport ++ "nonstiff." ++ dp54]
      $ libConstructor dp54 itArgs it],
  mandatoryStep $ callStep $ libMethod it "addStepHandler" [
    customObjArg (map ((apacheImport ++ "sampling.") ++) [sh, si]) stepHandler 
      (implementation sh [
        methodInfo "init" (map lockedParam [t0, y0, t]) Void [statementStep 
          (\cdch _ -> FAsg (head cdch) (Matrix [[idx (sy y0) (int 0)]]))],
        methodInfo "handleStep" (map lockedParam [interpolator, isLast]) Void [
          callStep $ libMethodWithResult interpolator "getInterpolatedState" 
            [] curr,
          appendCurrSol curr]])],
  mandatoryStep $ callStep $ libMethod it "integrate" (customObjArg 
    [apacheImport ++ fode] ode (implementation fode [
      constructorInfo [] [],
      methodInfo "getDimension" [] C.Integer [fixedReturn (int 1)],
      methodInfo "computeDerivatives" [
        lockedParam t, unnamedParam (Array Float), unnamedParam (Array Float)]
        Void [statementStep (\cdch e -> FAsgIndex (head cdch) 0 (head e))]]) : 
    [inlineArg Float, preDefinedArg currVals, inlineArg Float, 
      preDefinedArg currVals]),
  mandatoryStep $ statementStep (\cdch _ -> 
    FAsg (head cdch) (sy $ ccObjVar stepHandler (head cdch)))]

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

it, currVals, stepHandler, t0, y0, interpolator, isLast, curr :: CodeChunk
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

-- odeint (C++) --

odeint :: ExternalLibrary
odeint = externalLib [
  choiceSteps [
    [callStep $ libConstructor (odeNameSpace ++ rkdp5) [] rk,
    callStep $ libFunctionWithResult (odeNameSpace ++ "make_controlled") 
      [inlineArg Float, inlineArg Float, lockedArg (sy rk)] stepper],
    [callStep $ libConstructor (odeNameSpace ++ adamsBash) [] stepper]],
  mandatoryStep $ callWithImport "boost/numeric/odeint.hpp" $ libFunction 
    (odeNameSpace ++ "integrate_const") [
      lockedArg (sy stepper), 
      customObjArg [] ode (customClass [
        constructorInfo [] [],
        methodInfo "operator()" [unnamedParam (List Float),
          unnamedParam (List Float), lockedParam t] Void [
            statementStep (\cdch e -> FAsgIndex (head cdch) 0 (head e))]]),
      -- Need to declare variable holding initial value because odeint will update this variable at each step
      preDefinedArg odeintCurrVals,
      inlineArg Float, inlineArg Float, inlineArg Float, 
      customObjArg [] pop (customClass [
        constructorInfo [unnamedParam (List Float)] [],
        methodInfo "operator()" [lockedParam y, lockedParam t] Void
          [appendCurrSol y]])]]

odeNameSpace, rkdp5, adamsBash :: String
odeNameSpace = "boost::numeric::odeint::"
rkdp5 = "runge_kutta_dopri5<vector<double>>"
adamsBash = "adams_bashforth<3,vector<double>>"

odeintCurrVals, rk, stepper, pop :: CodeChunk
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
