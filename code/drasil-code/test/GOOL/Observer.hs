-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module GOOL.Observer (observer, observerName, printNum, x) where

import Drasil.GOOL (OOProg, VS, CS, FS, MS, FileSym(..), AttachmentSym(..),
  BodySym, BlockSym, oneLiner, TypeSym(..), PrintConsole(..), VariableSym(..),
  OOVariableSym, SelfSym(..), instanceVarSelf, Literal(..), VariableValue(..),
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
observer
  :: (OOProg r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ)
  => FS (r file)
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

-- | Makes a variable @x@.
x :: (TypeSym r typ, VariableSym r var typ) => VS (r var)
x = var "x" int

-- | Acces the @x@ attribute of @self@.
selfX ::
  ( TypeSym r typ
  , VariableSym r var typ
  , OOVariableSym r var val typ
  , SelfSym r var
  , VariableValue r var val
  )
  => VS (r var)
selfX = instanceVarSelf x

-- | Helper function to create the class.
helperClass
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r stvr attch vis var val
    , ClassSym r cls stvr mthd
    , OOMethodSym r mthd attch vis param bod var val typ
    , PrintConsole r stmt val
    , TypeSym r typ
    , Literal r val typ
    , VariableSym r var typ
    , OOVariableSym r var val typ
    , SelfSym r var
    , VariableValue r var val
    )
  => CS (r cls)
helperClass = buildClass Nothing [stateVar public instanceLevel x]
  [observerConstructor] [printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor
  ::
    ( BodySym r bod block
    , TypeSym r typ
    , VariableSym r var typ
    , OOMethodSym r mthd attch vis param bod var val typ
    , Literal r val typ
    )
  => MS (r mthd)
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , OOMethodSym r mthd attch vis param bod var val typ
    , AttachmentSym r attch
    , VisibilitySym r vis
    , PrintConsole r stmt val
    , VariableSym r var typ
    , OOVariableSym r var val typ
    , SelfSym r var
    , VariableValue r var val
    )
  => MS (r mthd)
printNumMethod = method printNum public instanceLevel void [] $
  oneLiner $ printLn $ valueOf selfX
