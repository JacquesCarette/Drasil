-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module GOOL.Observer (observer, observerName, printNum, x) where

import Drasil.GOOL (SFile, SVariable, SMethod, SClass, OOProg, FileSym(..),
  PermanenceSym(..), oneLiner, TypeSym(..), IOStatement(..), VariableSym(..),
  OOVariableSym(..), Literal(..), VariableValue(..), OOVariableValue,
  VisibilitySym(..), OOMethodSym(..), initializer, StateVarSym(..),
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
selfX :: (OOVariableSym r) => SVariable r
selfX = objVarSelf x

-- | Helper function to create the class.
helperClass :: (ClassSym r, IOStatement r, Literal r, OOVariableValue r) => SClass r
helperClass = buildClass Nothing [stateVar public dynamic x]
  [observerConstructor] [printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor :: (OOMethodSym r, Literal r) => SMethod r
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod :: (OOMethodSym r, IOStatement r, OOVariableValue r) => SMethod r
printNumMethod = method printNum public dynamic void [] $
  oneLiner $ printLn $ valueOf selfX
