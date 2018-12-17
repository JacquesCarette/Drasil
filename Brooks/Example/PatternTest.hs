module Example.PatternTest (patternTest) where

import New (
  RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ValueSym(..), ValueExpression(..), 
  MethodSym(..), ClassSym(..), ModuleSym(..))
import LanguageRenderer.NewJavaRenderer()
import Prelude hiding (return,print,log,exp,sin,cos,tan)

patternTest :: (RenderSym repr) => repr (RenderFile repr)
patternTest = fileDoc (buildModule "" ["Observer"] [] [] [patternTestClass])

patternTestClass :: (RenderSym repr) => repr (Class repr)
patternTestClass = mainClass "PatternTest" [] [patternTestMainMethod]

patternTestMainMethod :: (RenderSym repr) => repr (Method repr)
patternTestMainMethod = mainMethod (body [ (block [(varDec "n" int), (initState "myFSM" "Off"), (changeState "myFSM" "On")]),
  (checkState "myFSM" 
    [((litString "Off"), oneLiner (printStrLn "Off")), ((litString "On"), oneLiner (printStrLn "On"))] 
    (oneLiner (printStrLn "In"))),
  (runStrategy "myStrat" 
    [("myStrat", oneLiner (printStrLn "myStrat")), ("yourStrat", oneLiner (printStrLn "yourStrat"))]
    (Just (litInt 3)) (Just (var "n"))),
  (block [(varDecDef "obs1" (obj "Observer") (stateObj (obj "Observer") [])), (varDecDef "obs2" (obj "Observer") (stateObj (obj "Observer") []))]),
  (block [(initObserverList (listType static (obj "Observer")) [(var "obs1")]), (addObserver (obj "Observer") (var "obs2"))]),
  (notifyObservers "printNum" (listType static (obj "Observer")) [])])