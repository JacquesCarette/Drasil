-- | GOOL PatternTest module. Various tests to make sure equating and
-- the Observer class both work.
module PatternTest (patternTest) where

import GOOL.Drasil (GSProgram, VSType, SVariable, SValue, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), oneLiner, BlockSym(..),
  TypeSym(..), OOTypeSym(..), StatementSym(..), DeclStatement(..),
  IOStatement(..), initObserverList, addObserver, mainVar, OOVariableSym(..),
  ScopeSym(..), Literal(..), VariableValue(..), OOValueExpression(..),
  extNewObj, FunctionSym(..), GetSet(..), ObserverPattern(..),
  StrategyPattern(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Observer (observer, observerName, printNum, x)

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
n = mainVar nName int
obs1 = mainVar obs1Name observerType
obs2 = mainVar obs2Name observerType

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
  varDec n],

  runStrategy strat1
    [(strat1, oneLiner $ printStrLn strat1),
     (strat2, oneLiner $ printStrLn strat2)]
    (Just $ litInt 3) (Just n),

  block [
    varDecDef obs1 newObserver,
    varDecDef obs2 newObserver],

  block [
    initObserverList observerType [valueOf obs1] mainFn,
    addObserver (valueOf obs2) mainFn,
    notifyObservers (func printNum void []) observerType mainFn],

  block [
    valStmt $ set (valueOf obs1) x (litInt 10),
    print $ get (valueOf obs1) x]])
