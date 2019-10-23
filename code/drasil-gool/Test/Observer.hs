module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
observerName = "Observer"
observerDesc = "This is an arbitrary class acting as an Observer"
printNum = "printNum"

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

x :: (VariableSym repr) => repr (Variable repr)
x = var "x" int

selfX :: (VariableSym repr) => repr (Variable repr)
selfX = objVarSelf observerName x

helperClass :: (ClassSym repr) => repr (Class repr)
helperClass = pubClass observerName Nothing [stateVar public dynamic_ x]
  [observerConstructor, printNumMethod, getMethod observerName x, 
  setMethod observerName x]

observerConstructor :: (MethodSym repr) => repr (Method repr)
observerConstructor = constructor observerName [] $ oneLiner $ assign selfX 
  (litInt 5)

printNumMethod :: (MethodSym repr) => repr (Method repr)
printNumMethod = method printNum observerName public dynamic_ void [] $
  oneLiner $ printLn $ valueOf selfX