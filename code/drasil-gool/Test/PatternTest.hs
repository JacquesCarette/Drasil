module Test.PatternTest (patternTest) where

import GOOL.Drasil (
  ProgramSym(..), RenderSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), TypeSym(..), 
  StatementSym(..), ControlStatementSym(..), VariableSym(..), ValueSym(..),
  ValueExpression(..), FunctionSym(..), MethodSym(..), ModuleSym(..), GOOLState)
import Control.Monad.State (State)
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.Observer (observer, observerName, printNum, x)

progName, fsmName, offState, onState, noState, strat1, strat2, obs1Name, 
  obs2Name, nName :: String
progName = "PatternTest"
fsmName = "myFSM"
offState = "Off"
onState = "On"
noState = "Neither"
strat1 = "myStrat"
strat2 = "yourStrat"
obs1Name = "obs1"
obs2Name = "obs2"
nName = "n"

observerType :: (TypeSym repr) => repr (Type repr)
observerType = obj observerName

n, obs1, obs2 :: (VariableSym repr) => repr (Variable repr)
n = var nName int
obs1 = var obs1Name observerType
obs2 = var obs2Name observerType

newObserver :: (ValueExpression repr) => repr (Value repr)
newObserver = extNewObj observerName observerType []

patternTest :: (ProgramSym repr) => State GOOLState (repr (Program repr))
patternTest = prog progName [fileDoc (buildModule progName [observerName] [patternTestMainMethod] []), observer]

patternTestMainMethod :: (MethodSym repr) => State GOOLState (repr (Method repr))
patternTestMainMethod = mainFunction (body [block [
  varDec n,
  initState fsmName offState, 
  changeState fsmName onState,
  checkState fsmName 
    [(litString offState, 
      oneLiner $ printStrLn offState), 
     (litString onState, 
       oneLiner $ printStrLn onState)] 
    (oneLiner $ printStrLn noState)],

  runStrategy strat1
    [(strat1, oneLiner $ printStrLn strat1), 
     (strat2, oneLiner $ printStrLn strat2)]
    (Just $ litInt 3) (Just n),

  block [
    varDecDef obs1 newObserver, 
    varDecDef obs2 newObserver],

  block [
    initObserverList observerType [valueOf obs1], 
    addObserver $ valueOf obs2,
    notifyObservers (func printNum void []) observerType],
    
  block [
    valState $ set (valueOf obs1) x (litInt 10),
    print $ get (valueOf obs1) x]])