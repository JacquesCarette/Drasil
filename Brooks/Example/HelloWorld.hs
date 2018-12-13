module Example.HelloWorld (main) where

import New (Declaration, StateVar, Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..), InputTypeSym(..),
  BodySym(..), BlockSym(..), ControlStatementSym(..), StateTypeSym(..), 
  PreStatementSym(..), StatementSym(..), IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), 
  ValueSym(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), ClassSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode (map unJC [helloWorld, patternTest]) ["HelloWorld", "PatternTest"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

patternTest :: (RenderSym repr) => repr (RenderFile repr)
patternTest = fileDoc ( mainMethod (bodyBlockState [ (statements [(varDec "n" int), (initState "myFSM" "Off"), (changeState "myFSM" "On")]),
  (checkState "myFSM" 
    [((litString "Off"), oneLiner (printStrLn "Off")), ((litString "On"), oneLiner (printStrLn "On"))] 
    (oneLiner (printStrLn "In"))),
  (runStrategy "myStrat" 
    [("myStrat", oneLiner (printStrLn "myStrat")), ("yourStrat", oneLiner (printStrLn "yourStrat"))]
    (Just (litInt 3)) (Just (var "n"))),
  (statements [(initObserverList (intListType static) [(litInt 1), (litInt 2)]), (addObserver (intListType static) (litInt 3))]),
  (notifyObservers "addNums" (intListType static) [(litInt 2), (litInt 5)])]))

helloWorld :: (RenderSym repr) => repr (RenderFile repr)
helloWorld = fileDoc ( mainMethod (bodyBlockState [ helloInitVariables, helloListSlice,
  ifCond [(litFalse, bodyStatements [(varDec "dummy" string)]),
    (litTrue, helloIfBody)] helloElseBody,
  switch (var "a") [((litInt 5), (oneLiner ("b" &.= (litInt 10)))), 
    ((litInt 0), (oneLiner ("b" &.= (litInt 5))))]
    (oneLiner ("b" &.= (litInt 0))),
  helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch, goodBye1, helloFileRead, goodBye2]))

helloInitVariables :: (RenderSym repr) => repr (Statement repr)
helloInitVariables = statements [ (comment "Initializing variables"),
  (varDec "a" int), 
  (varDecDef "b" int (litInt 5)),
  (listDecDef "myOtherList" (floatListType static) [(litFloat 1.0), (litFloat 1.5)]),
  ("a" &.= (objAccess (var "myOtherList") listSize)),
  (valState (objAccess (var "myOtherList") (listAdd (litInt 2) (litFloat 2.0)))),
  (valState (objAccess (var "myOtherList") (listAppend (litFloat 2.5)))),
  (valState (objAccess (var "myOtherList") (listExtendFloat))),
  (varDec "e" float),
  ("e" &.= (objAccess (var "myOtherList") (listAccess (litInt 1)))),
  (valState (objAccess (var "myOtherList") (listSet (litInt 1) (litFloat 17.4)))),
  (listDec "myName" 7 (listType static string)),
  (stringSplit ' ' (var "myName") (litString "Brooks Mac")),
  (printLn (string) (var "myName")),
  (listDec "mySlicedList" 2 $ floatListType static)]

helloListSlice :: (RenderSym repr) => repr (Statement repr)
helloListSlice = (listSlice (floatListType static) (var "mySlicedList") (var "myOtherList") (Just (litInt 1)) (Just (litInt 3)) Nothing)


helloIfBody :: (RenderSym repr) => repr (Body repr)
helloIfBody = addComments "If body" (body [
  block [
    varDec "c" int,
    varDec "d" int,
    assign (var "a") (litInt 5),
    (var "b") &= ((var "a") #+ (litInt 2)),
    "c" &.= ((var "b") #+ (litInt 3)),
    (var "d") &=. "b",
    (var "d") &-= (var "a"),
    "c" &.-= (var "d"),
    (var "b") &+= (litInt 17),
    "c" &.+= (litInt 17),
    (&++) (var "a"),
    (&.++) "d",
    (&~-) (var "c"),
    (&.~-) "b",

    listDec "myList" 5 (intListType static),
    objDecDef "myObj" char (litChar 'o'),
    constDecDef "myConst" string (litString "Imconstant"),

    printLn (int) (var "a"),
    printLn (int) (var "b"),
    printLn (int) (var "c"),
    printLn (int) (var "d"),
    printLnList (float) (var "myOtherList"),
    printLnList (float) (var "mySlicedList"),
    
    getInput inputInt (var "d"),
    discardInput inputInt],
  
  block [
    printLn (bool) (litTrue ?&& litFalse),
    printLn (bool) (litTrue ?|| litFalse),
    printLn (bool) (litTrue ?&& ((?!) litFalse)),
    printLn (bool) ((?!) (litTrue ?&& litTrue)),
    printLn (int) ((litInt 6) #+ (litInt 2)),
    printLn (int) ((litInt 6) #- (litInt 2)),
    printLn (int) ((litInt 6) #* (litInt 2)),
    printLn (int) ((litInt 6) #/ (litInt 2)),
    printLn (int) ((litInt 6) #% (litInt 4)),
    printLn (int) ((litInt 6) #^ (litInt 2)),
    printLn (int) ((litInt 6) #+ ((litInt 2) #* (litInt 3))),
    printLn (float) (csc (litFloat 1.0)),
    printLn (float) (sec (litFloat 1.0)),
    printLn (float) (cot (litFloat 1.0))]])
    -- printLn (int) (notNull (var "a")),
    -- printLn (int) (var "a"),
    -- printLn (int) (arg 5),
    -- printLn (int) (extVar "Lib" "var"),
    -- printLn (int) (self),
    -- printLn (int) (objVarSelf "thisOne"),
    -- printLn (int) (objVar (var "outer") (var "inner")),
    -- printLn (int) (inlineIf (litTrue) (litInt 5) (litInt 0)),
    -- printLn (int) (funcApp "myFunc" [(var "arg1"), (var "arg2")]),
    -- printLn (int) (extFuncApp "myLib" "myFunc" [(var "arg1"), (var "arg2")]),
    -- printLn (int) (stateObj bool [(var "arg1"), (var "arg2")]),
    -- printLn (int) (listStateObj bool [(var "arg1"), (var "arg2")])

helloElseBody :: (RenderSym repr) => repr (Body repr)
helloElseBody = bodyStatements [
  printStrLn "Hello, world",
  printLn (string) (litString " too"),
  printStr "boo",
  print (bool) litTrue,
  printLn (float) defaultFloat,
  print (int) (litInt 0),
  print (char) (litChar 'c'),
  printLn (bool) ((?!) litTrue),
  printLn (int) ((#~) (litInt 1)),
  printLn (float) ((#/^) (litFloat 4.0)),
  printLn (int) ((#|) (litInt (-4))),
  printLn (float) (log ((#~) (litFloat 2.0))),
  printLn (float) (ln (litFloat 2.0)),
  printLn (float) (exp (litFloat 2.0)),
  printLn (float) (sin (litFloat 2.0)),
  printLn (float) (cos (litFloat 2.0)),
  printLn (float) (tan (litFloat 2.0)),
  printLn (float) (tan (litFloat 2.0))]

helloForLoop :: (RenderSym repr) => repr (Statement repr)
helloForLoop = for (varDecDef "i" int (litInt 0)) ((var "i") ?< (litInt 10)) ((&.++) "i")
  (oneLiner (printLn (int) (var "i")))

helloWhileLoop :: (RenderSym repr) => repr (Statement repr)
helloWhileLoop = while (var "a" ?< (litInt 13)) (bodyStatements [printStrLn "Hello", ((&.++) "a")]) 

helloForEachLoop :: (RenderSym repr) => repr (Statement repr)
helloForEachLoop = forEach "num" (float) (listVar "myOtherList" (float)) 
  (oneLiner (printLn (float) (var "num")))

helloTryCatch :: (RenderSym repr) => repr (Statement repr)
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught error"))

goodBye1 :: (RenderSym repr) => repr (Statement repr)
goodBye1 = statements [
  (varDec "f" float),
  ("f" &.= (castObj (cast float int) (var "e"))),
  (varDec "file" (obj "Scanner")),
  (openFileR (var "file") (litString "filename")),
  (varDec "fileContents" string),
  (getFileInputLine (var "file") (var "fileContents")),
  (discardFileLine (var "file"))]

helloFileRead :: (RenderSym repr) => repr (Statement repr)
helloFileRead = (getFileInputAll (var "file") (var "fileContents"))

goodBye2 :: (RenderSym repr) => repr (Statement repr)
goodBye2 = statement (closeFile (var "file"))