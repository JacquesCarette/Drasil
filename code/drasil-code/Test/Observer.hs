module Test.Observer (observer, observerName, printNum, x) where

import GOOL.Drasil (SFile, SVariable, SMethod, SClass, OOProg, FileSym(..), 
  PermanenceSym(..), oneLiner, TypeSym(..), IOStatement(..), VariableSym(..), 
  Literal(..), VariableValue(..), ScopeSym(..), MethodSym(..), initializer, StateVarSym(..), 
  ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
observerName = "Observer"
observerDesc = "This is an arbitrary class acting as an Observer"
printNum = "printNum"

observer :: (OOProg r) => SFile r
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

x :: (VariableSym r) => SVariable r
x = var "x" int

selfX :: (VariableSym r) => SVariable r
selfX = objVarSelf x

helperClass :: (ClassSym r, IOStatement r, Literal r, VariableValue r) => SClass r
helperClass = buildClass Nothing [stateVar public dynamic x]
  [observerConstructor, printNumMethod, getMethod x, setMethod x]

observerConstructor :: (MethodSym r, Literal r) => SMethod r
observerConstructor = initializer [] [(x, litInt 5)]

printNumMethod :: (MethodSym r, IOStatement r, VariableValue r) => SMethod r
printNumMethod = method printNum public dynamic void [] $
  oneLiner $ printLn $ valueOf selfX