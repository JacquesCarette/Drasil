module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil.Symantics (
  RenderSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, printNum :: String
observerName = "Observer"
printNum = "printNum"

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule observerName [] [] [docClass 
  "This is an arbitrary class acting as an Observer"
  helperClass])

x :: (VariableSym repr) => repr (Variable repr)
x = var "x" int

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass observerName Nothing [stateVar public dynamic_ x]
  [observerConstructor, printNumMethod, getMethod observerName x, 
  setMethod observerName x]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor observerName [] (oneLiner (assign (objVarSelf observerName "x" int) (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method printNum observerName public dynamic_ void [] 
  (oneLiner (printLn (valueOf $ objVarSelf observerName "x" int)))