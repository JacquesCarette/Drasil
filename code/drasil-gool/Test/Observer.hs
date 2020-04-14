module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil (SFile, SVariable, SMethod, SClass, ProgramSym, FileSym(..), 
  PermanenceSym(..), oneLiner, TypeSym(..), IOStatement(..), VariableSym(..), 
  Literal(..), VariableValue(..), ScopeSym(..), MethodSym(..), initializer, StateVarSym(..), 
  ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
observerName = "Observer"
observerDesc = "This is an arbitrary class acting as an Observer"
printNum = "printNum"

observer :: (ProgramSym repr) => SFile repr
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

x :: (VariableSym repr) => SVariable repr
x = var "x" int

selfX :: (VariableSym repr) => SVariable repr
selfX = objVarSelf x

helperClass :: (ClassSym repr, IOStatement repr, Literal repr, VariableValue repr) => SClass repr
helperClass = buildClass observerName Nothing [stateVar public dynamic x]
  [observerConstructor, printNumMethod, getMethod x, setMethod x]

observerConstructor :: (MethodSym repr, Literal repr) => SMethod repr
observerConstructor = initializer [] [(x, litInt 5)]

printNumMethod :: (MethodSym repr, IOStatement repr, VariableValue repr) => SMethod repr
printNumMethod = method printNum public dynamic void [] $
  oneLiner $ printLn $ valueOf selfX