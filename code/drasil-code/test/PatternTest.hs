-- | GOOL PatternTest module. Various tests to make sure equating and
-- the Observer class both work.
module PatternTest (patternTest) where

import GOOL.Drasil (GSProgram, VSType, SVariable, SValue, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), oneLiner, BlockSym(..),
  TypeSym(..), StatementSym(..), AssignStatement, DeclStatement(..),
  IOStatement(..), initState, changeState, initObserverList,
  addObserver, VariableSym(..), Literal(..), VariableValue(..),
  ValueExpression(..), extNewObj, FunctionSym(..), GetSet(..), List, StatePattern(..), ObserverPattern(..), StrategyPattern(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Observer (observer, observerName, printNum, x)

-- | Variables, program names, and used strings within the program.
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

-- | Initialize Observer variables.
observerType :: (TypeSym r) => VSType r
observerType = obj observerName

-- | Variables used in the generated code.
n, obs1, obs2 :: (VariableSym r) => SVariable r
n = var nName int
obs1 = var obs1Name observerType
obs2 = var obs2Name observerType

-- | New Observer object.
newObserver :: (ValueExpression r) => SValue r
newObserver = extNewObj observerName observerType []

-- | Creates the pattern test program.
patternTest :: (OOProg r) => GSProgram r
patternTest = prog progName "" [fileDoc (buildModule progName []
  [patternTestMainMethod] []), observer]

-- | Creates the main function for PatternTest.
patternTestMainMethod :: (MethodSym r, AssignStatement r, DeclStatement r,
  IOStatement r, Literal r, VariableValue r, ValueExpression r, GetSet r,
  List r, StatePattern r, ObserverPattern r, StrategyPattern r) => SMethod r
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
    valStmt $ set (valueOf obs1) x (litInt 10),
    print $ get (valueOf obs1) x]])
