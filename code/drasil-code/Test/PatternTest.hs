module Test.PatternTest (patternTest) where

import Language.Drasil.Code (
  PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..), ValueSym(..), ValueExpression(..), 
  MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.Observer (observer)

patternTest :: (PackageSym repr) => repr (Package repr)
patternTest = packMods "PatternTest" [fileDoc (buildModule "PatternTest" ["Observer"] [] [patternTestMainMethod] []), observer]

patternTestMainMethod :: (RenderSym repr) => repr (Method repr)
patternTestMainMethod = mainMethod "PatternTest" (body [block [
  varDec $ var "n" int,
  initState "myFSM" "Off", 
  changeState "myFSM" "On",
  checkState "myFSM" 
    [(litString "Off", 
      oneLiner (printStrLn "Off")), 
      (litString "On", oneLiner (printStrLn "On"))] 
    (oneLiner (printStrLn "In"))],

  runStrategy "myStrat" 
    [("myStrat", oneLiner (printStrLn "myStrat")), 
      ("yourStrat", oneLiner (printStrLn "yourStrat"))]
    (Just (litInt 3)) (Just (var "n" int)),

  block [
    varDecDef "obs1" (obj "Observer") (extStateObj "Observer" (obj "Observer") []), 
    varDecDef "obs2" (obj "Observer") (extStateObj "Observer" (obj "Observer") [])],
  block [
    initObserverList (listType static_ (obj "Observer")) [var "obs1" (obj "Observer")], 
    addObserver (obj "Observer") (var "obs2" (obj "Observer")),
    notifyObservers void "printNum" (listType static_ (obj "Observer")) []]])