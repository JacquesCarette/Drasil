module Test.Observer (observer, x) where

import GOOL.Drasil (RenderSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..), Other)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observer :: (RenderSym repr) => repr (RenderFile repr)
observer = fileDoc (buildModule "Observer" [] [] [docClass 
  "This is an arbitrary class acting as an Observer"
  helperClass])

x :: (RenderSym repr) => repr (Variable repr Other)
x = var "x" int

helperClass :: (RenderSym repr) => repr (Class repr)
helperClass = pubClass "Observer" Nothing [stateVar 0 public dynamic_ x]
  [observerConstructor, printNumMethod, getMethod "Observer" x, 
  setMethod "Observer" x]

observerConstructor :: (RenderSym repr) => repr (Method repr)
observerConstructor = constructor "Observer" [] (oneLiner (assign (objVarSelf "Observer" "x" int) (litInt 5)))

printNumMethod :: (RenderSym repr) => repr (Method repr)
printNumMethod = method "printNum" "Observer" public dynamic_ void [] 
  (oneLiner (printLn (valueOf $ objVarSelf "Observer" "x" int)))