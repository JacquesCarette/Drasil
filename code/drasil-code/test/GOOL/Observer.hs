-- | Part of the PatternTest GOOL tests. Defines an Observer class.
module GOOL.Observer (observer, observerName, printNum, x) where

import Drasil.GOOL (SVariable, Class, OOProg, CS, FS, MS, FileSym(..),
  AttachmentSym(..), BodySym, BlockSym, oneLiner, TypeSym(..), PrintConsole(..),
  VariableSym(..), OOVariableSym, SelfSym(..), instanceVarSelf, Literal(..),
  VariableValue(..), VisibilitySym(..), OOMethodSym(..), initializer,
  StateVarSym(..), ClassSym(..), ModuleSym(..))
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
  :: (OOProg r vis typ val stmt mthd stvr attch prg file mod bod block)
  => FS (r file)
observer = fileDoc (buildModule observerName [] [] [docClass observerDesc
  helperClass])

-- | Makes a variable @x@.
x :: (TypeSym r typ, VariableSym r typ) => SVariable r
x = var "x" int

-- | Acces the @x@ attribute of @self@.
selfX ::
  ( TypeSym r typ
  , VariableSym r typ
  , OOVariableSym r typ val
  , SelfSym r
  , VariableValue r val
  )
  => SVariable r
selfX = instanceVarSelf x

-- | Helper function to create the class.
helperClass
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , AttachmentSym r attch
    , VisibilitySym r vis
    , StateVarSym r vis val stvr attch
    , ClassSym r mthd stvr
    , OOMethodSym r vis typ val mthd attch bod
    , PrintConsole r val stmt
    , TypeSym r typ
    , Literal r typ val
    , VariableSym r typ
    , OOVariableSym r typ val
    , SelfSym r
    , VariableValue r val
    )
  => CS (r Class)
helperClass = buildClass Nothing [stateVar public instanceLevel x]
  [observerConstructor] [printNumMethod, getMethod x, setMethod x]

-- | Default value for observer class is 5.
observerConstructor
  ::
    ( BodySym r bod block
    , TypeSym r typ
    , VariableSym r typ
    , OOMethodSym r vis typ val mthd attch bod
    , Literal r typ val
    )
  => MS (r mthd)
observerConstructor = initializer [] [(x, litInt 5)]

-- | Create the @printNum@ method.
printNumMethod
  ::
    ( BlockSym r block stmt
    , BodySym r bod block
    , TypeSym r typ
    , OOMethodSym r vis typ val mthd attch bod
    , AttachmentSym r attch
    , VisibilitySym r vis
    , PrintConsole r val stmt
    , VariableSym r typ
    , OOVariableSym r typ val
    , SelfSym r
    , VariableValue r val
    )
  => MS (r mthd)
printNumMethod = method printNum public instanceLevel void [] $
  oneLiner $ printLn $ valueOf selfX
