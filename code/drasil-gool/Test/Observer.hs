module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil (
  ProgramSym, FileSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..), FS, MS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
observerName = "Observer"
observerDesc = "This is an arbitrary class acting as an Observer"
printNum = "printNum"

observer :: (ProgramSym repr) => FS (repr (RenderFile repr))
observer = fileDoc (buildModule observerName [] [docClass observerDesc
  helperClass])

x :: (VariableSym repr) => MS (repr (Variable repr))
x = var "x" int

selfX :: (VariableSym repr) => MS (repr (Variable repr))
selfX = objVarSelf observerName x

helperClass :: (ClassSym repr) => FS (repr (Class repr))
helperClass = pubClass observerName Nothing [stateVar public dynamic_ x]
  [observerConstructor, printNumMethod, getMethod observerName x, 
  setMethod observerName x]

observerConstructor :: (MethodSym repr) => MS (repr (Method repr))
observerConstructor = constructor observerName [] $ oneLiner $ assign selfX 
  (litInt 5)

printNumMethod :: (MethodSym repr) => MS (repr (Method repr))
printNumMethod = method printNum observerName public dynamic_ void [] $
  oneLiner $ printLn $ valueOf selfX