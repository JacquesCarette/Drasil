module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil (
  ProgramSym, FileSym(..), PermanenceSym(..), BodySym(..), TypeSym(..), 
  StatementSym(..), VariableSym(..), ValueSym(..), ScopeSym(..), 
  MethodSym(..), initializer, StateVarSym(..), ClassSym(..), ModuleSym(..), FS, 
  CS, MS, VS)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
observerName = "Observer"
observerDesc = "This is an arbitrary class acting as an Observer"
printNum = "printNum"

observer :: (ProgramSym repr) => FS (repr (RenderFile repr))
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

x :: (VariableSym repr) => VS (repr (Variable repr))
x = var "x" int

selfX :: (VariableSym repr) => VS (repr (Variable repr))
selfX = objVarSelf x

helperClass :: (ClassSym repr) => CS (repr (Class repr))
helperClass = pubClass observerName Nothing [stateVar public dynamic x]
  [observerConstructor, printNumMethod, getMethod x, setMethod x]

observerConstructor :: (MethodSym repr) => MS (repr (Method repr))
observerConstructor = initializer [] [(x, litInt 5)]

printNumMethod :: (MethodSym repr) => MS (repr (Method repr))
printNumMethod = method printNum public dynamic void [] $
  oneLiner $ printLn $ valueOf selfX