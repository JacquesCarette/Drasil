-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module Observer (observer, observerName, printNum, x) where

import GOOL.Drasil (SFile, SVariable, SMethod, SClass, OOProg, FileSym(..),
  PermanenceSym(..), oneLiner, TypeSym(..), IOStatement(..), VariableSym(..),
  Literal(..), VariableValue(..), ScopeSym(..), MethodSym(..), initializer, StateVarSym(..),
  ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
-- | Class name.
observerName = "Observer"
-- | Class description.
observerDesc = "This is an arbitrary class acting as an Observer"
-- | A method name within the class.
printNum = "printNum"

-- | Creates the observer class.
observer :: (OOProg r) => SFile r
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

-- | Makes a variable @x@.
x :: (VariableSym r) => SVariable r
x = var "x" int

-- | Acces the @x@ attribute of @self@.
selfX :: (VariableSym r) => SVariable r
selfX = objVarSelf x

-- | Helper function to create the class.
helperClass :: (ClassSym r, IOStatement r, Literal r, VariableValue r) => SClass r
helperClass = buildClass Nothing [stateVar public dynamic x]
  [observerConstructor, printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor :: (MethodSym r, Literal r) => SMethod r
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod :: (MethodSym r, IOStatement r, VariableValue r) => SMethod r
printNumMethod = method printNum public dynamic void [] $
  oneLiner $ printLn $ valueOf selfX
