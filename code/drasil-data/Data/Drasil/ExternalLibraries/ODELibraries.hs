module Data.Drasil.ExternalLibraries.ODELibraries (scipyODE) where

import Language.Drasil

import Language.Drasil.Code (FuncStmt(..), ExternalLibrary, FunctionInterface, 
  Argument, externalLib, mandatoryStep, choiceStep, libMethod, 
  libFunctionWithResult, loopConditionMethod, loopedMethod, lockedArg, 
  lockedNamedArg, inlineArg, inlineNamedArg, functionArg, CodeChunk, codevar, 
  implCQD)

import GOOL.Drasil (CodeType(..))

-- SciPy -- 

scipyODE :: ExternalLibrary
scipyODE = externalLib [
  mandatoryStep $ libFunctionWithResult "scipy.integrate.ode" [
    functionArg f [Float, Float] (\es -> FRet $ Matrix [es])] r,
  choiceStep [
    setIntegratorMethod [vode, methodArg "adams", atol, rtol],
    setIntegratorMethod [vode, methodArg "bdf", atol, rtol],
    setIntegratorMethod [lockedArg (str "dopri45"), atol, rtol]],
  mandatoryStep $ libMethod r "set_initial_value" [inlineArg Float],
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
  "Current independent variable value" "Current independent variable values") 
  Nothing Float (Label "r.t") Nothing
ry = codevar $ implCQD "r_y_scipy" (nounPhrase 
  "Current dependent variable value" "Current dependent variable values") 
  Nothing (List Float) (Label "r.y") Nothing
