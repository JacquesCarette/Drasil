-- | GOOL PatternTest module. Various tests to make sure equating and
-- the Observer class both work.
module GOOL.PatternTest (patternTest) where

import Drasil.GOOL (GSProgram, VSType, SVariable, SValue, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), oneLiner, BlockSym(..),
  TypeSym(..), OOTypeSym(..), StatementSym(..), DeclStatement(..),
  IOStatement(..), initObserverList, addObserver, VariableSym(var),
  OOVariableSym(..), ScopeSym(..), Literal(..), VariableValue(..),
  OOValueExpression(..), extNewObj, OOFunctionSym(..), GetSet(..),
  ObserverPattern(..), StrategyPattern(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import GOOL.Observer (observer, observerName, printNum, x)

-- | Variables, program names, and used strings within the program.
progName, strat1, strat2, obs1Name,
  obs2Name, nName :: String
progName = "PatternTest"
strat1 = "myStrat"
strat2 = "yourStrat"
obs1Name = "obs1"
obs2Name = "obs2"
nName = "n"

-- | Initialize Observer variables.
observerType :: (OOTypeSym r) => VSType r
observerType = obj observerName

-- | Variables used in the generated code.
n, obs1, obs2 :: (OOVariableSym r) => SVariable r
n = var nName int
obs1 = var obs1Name observerType
obs2 = var obs2Name observerType

-- | New Observer object.
newObserver :: (OOValueExpression r) => SValue r
newObserver = extNewObj observerName observerType []

-- | Creates the pattern test program.
patternTest :: (OOProg r) => GSProgram r
patternTest = prog progName "" [fileDoc (buildModule progName []
  [patternTestMainMethod] []), observer]

-- | Creates the main function for PatternTest.
patternTestMainMethod :: (OOProg r) => SMethod r
patternTestMainMethod = mainFunction (body [block [
  varDec n], --mainFn

  runStrategy strat1
    [(strat1, oneLiner $ printStrLn strat1),
     (strat2, oneLiner $ printStrLn strat2)]
    (Just $ litInt 3) (Just n),

  block [
    varDecDef obs1 newObserver, --mainFn
    varDecDef obs2 newObserver], --mainFn

  block [
    initObserverList observerType [valueOf obs1] mainFn,
    addObserver (valueOf obs2),
    notifyObservers (func printNum void []) observerType mainFn],

  block [
    valStmt $ set (valueOf obs1) x (litInt 10),
    print $ get (valueOf obs1) x]])
