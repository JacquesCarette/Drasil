-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module GOOL.Observer (observer, observerName, printNum, x) where

import Drasil.GOOL (SFile, SVariable, SMethod, SClass, OOProg, FileSym(..),
  AttachmentSym(..), oneLiner, TypeSym(..), IOStatement(..), VariableSym(..),
  SelfSym(..), instanceVarSelf, Literal(..), VariableValue(..), OOVariableValue,
  VisibilitySym(..), OOMethodSym(..), initializer, StateVarSym(..), ClassSym(..),
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

observerName, observerDesc, printNum :: String
-- | Class name.
observerName = "Observer"
-- | Class description.
observerDesc = "This is an arbitrary class acting as an Observer"
-- | A method name within the class.
printNum = "printNum"

-- | Creates the observer class.
observer :: (OOProg r tp vis smt) => SFile r
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

-- | Makes a variable @x@.
x :: (VariableSym r tp) => SVariable r
x = var "x" int

-- | Acces the @x@ attribute of @self@.
selfX :: (SelfSym r tp, VariableValue r tp) => SVariable r
selfX = instanceVarSelf x

-- | Helper function to create the class.
helperClass :: (ClassSym r tp vis smt, IOStatement r tp smt, Literal r tp,
  OOVariableValue r tp) => SClass r
helperClass = buildClass Nothing [stateVar public instanceLevel x]
  [observerConstructor] [printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor :: (OOMethodSym r tp vis smt, Literal r tp) => SMethod r
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod :: (OOMethodSym r tp vis smt, IOStatement r tp smt,
  OOVariableValue r tp) => SMethod r
printNumMethod = method printNum public instanceLevel void [] $
  oneLiner $ printLn $ valueOf selfX
