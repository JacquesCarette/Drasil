module Example.PatternTest (patternTest) where

import New (Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..), InputTypeSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..),
  FunctionSym(..), SelectorFunction(..), ScopeSym(..), MethodTypeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

patternTest :: (RenderSym repr) => repr (RenderFile repr)
patternTest = fileDoc (buildModule "" [] [] [] [patternTestClass])

patternTestClass :: (RenderSym repr) => repr (Class repr)
patternTestClass = mainClass "PatternTest" [] [patternTestMainMethod, addNums]

patternTestMainMethod :: (RenderSym repr) => repr (Method repr)
patternTestMainMethod = mainMethod (body [ (block [(varDec "n" int), (initState "myFSM" "Off"), (changeState "myFSM" "On")]),
  (checkState "myFSM" 
    [((litString "Off"), oneLiner (printStrLn "Off")), ((litString "On"), oneLiner (printStrLn "On"))] 
    (oneLiner (printStrLn "In"))),
  (runStrategy "myStrat" 
    [("myStrat", oneLiner (printStrLn "myStrat")), ("yourStrat", oneLiner (printStrLn "yourStrat"))]
    (Just (litInt 3)) (Just (var "n"))),
  (block [(listDec "obs1" 1 (intListType dynamic)), (listDec "obs2" 1 (intListType dynamic))]),
  (block [(initObserverList (listType static (intListType dynamic)) [(var "obs1")]), (addObserver (intListType static) (var "obs2"))]),
  (notifyObservers "add" (listType static (intListType dynamic)) [funcApp "addNums" [(litInt 2), (litInt 5)]])])

addNums :: (RenderSym repr) => repr (Method repr)
addNums = method "addNums" public static (mState int) [(stateParam "num1" int), (stateParam "num2" int)]
  (bodyStatements [varDecDef "sumNum" int ((var "num1") #+ (var "num2")), 
    returnVar "sumNum"])