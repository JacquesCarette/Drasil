-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module GOOL.Observer (observer, observerName, printNum, x) where

import Drasil.GOOL (SVariable, Class, OOProg, CS, FS, MS, FileSym(..),
  AttachmentSym(..), oneLiner, TypeSym(..), PrintConsole(..), VariableSym(..),
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
observer :: (OOProg r vis stmt mthd stvr attch prg file mod) => FS (r file)
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

-- | Makes a variable @x@.
x :: (VariableSym r) => SVariable r
x = var "x" int

-- | Acces the @x@ attribute of @self@.
selfX :: (SelfSym r, VariableValue r) => SVariable r
selfX = instanceVarSelf x

-- | Helper function to create the class.
helperClass :: (ClassSym r vis stmt mthd stvr attch, PrintConsole r stmt, Literal r,
  OOVariableValue r) => CS (r Class)
helperClass = buildClass Nothing [stateVar public instanceLevel x]
  [observerConstructor] [printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor :: (OOMethodSym r vis stmt mthd attch, Literal r) => MS (r mthd)
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod :: (OOMethodSym r vis stmt mthd attch, PrintConsole r stmt,
  OOVariableValue r) => MS (r mthd)
printNumMethod = method printNum public instanceLevel void [] $
  oneLiner $ printLn $ valueOf selfX
