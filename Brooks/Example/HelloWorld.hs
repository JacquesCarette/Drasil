module Example.HelloWorld (helloWorld) where

import New (
  RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..),  ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  ScopeSym(..), MethodTypeSym(..), ParameterSym(..), MethodSym(..), 
  StateVarSym(..), ClassSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)

helloWorld :: (RenderSym repr) => repr (RenderFile repr)
helloWorld = fileDoc (buildModule "HelloWorld" [] [] [] [helloWorldClass])

helloWorldClass :: (RenderSym repr) => repr (Class repr)
helloWorldClass = pubClass "HelloWorld" Nothing [stateVar 0 "greeting" private static string] [doubleAndAdd,
  mainMethod (body [ helloInitVariables, helloListSlice,
    block [ifCond [((var "b") ?>= (litInt 6), bodyStatements [(varDecDef "dummy" string (litString "dummy"))]),
      ((var "b") ?== (litInt 5), helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]])]

helloInitVariables :: (RenderSym repr) => repr (Block repr)
helloInitVariables = block [ (comment "Initializing variables"),
  (varDec "a" int), 
  (varDecDef "b" int (litInt 5)),
  (listDecDef "myOtherList" (floatListType static) [(litFloat 1.0), (litFloat 1.5)]),
  (varDecDef "oneIndex" int (indexOf (var "myOtherList") (litFloat 1.0))),
  (printLn int (var "oneIndex")),
  ("a" &.= (listSizeAccess (var "myOtherList"))),
  (valState (objAccess (var "myOtherList") (listAdd (litInt 2) (litFloat 2.0)))),
  (valState (objAccess (var "myOtherList") (listAppend (litFloat 2.5)))),
  (valState (objAccess (var "myOtherList") (listExtendFloat))),
  (varDec "e" float),
  ("e" &.= (objAccess (var "myOtherList") (listAccess (litInt 1)))),
  (valState (objAccess (var "myOtherList") (listSet (litInt 1) (litFloat 17.4)))),
  (listDec "myName" 7 (listType static string)),
  stringSplit ' ' (var "myName") (litString "Brooks Mac"),
  (printLnList (string) (var "myName")),
  (listDec "boringList" 5 boolListType),
  (valState $ listPopulateAccess (var "boringList") (listPopulateBool (litInt 5))),
  (printLnList bool (var "boringList")),
  (listDec "mySlicedList" 2 $ floatListType static)]

helloListSlice :: (RenderSym repr) => repr (Block repr)
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
    
    printStrLn "Type an int",
    getIntInput (var "d"),
    printStrLn "Type another",
    discardInput],
  
  block [
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
    printLn (float) (log (litFloat 2.0)),
    multi [printLn (float) (ln (litFloat 2.0)),
    printLn (float) (exp ((#~) (litFloat 2.0))),
    printLn (float) (sin (litFloat 2.0)),
    printLn (float) (cos (litFloat 2.0)),
    printLn (float) (tan (litFloat 2.0))],
    printLn (float) (tan (litFloat 2.0)),
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
    printLn (int) (var "a"),
    printLn (int) (inlineIf (litTrue) (litInt 5) (litInt 0)),
    printLn (float) (cot (litFloat 1.0))]])

helloElseBody :: (RenderSym repr) => repr (Body repr)
helloElseBody = bodyStatements [
  printStrLn "These will print but (by intention) not compile"]
  -- printLn (int) (notNull (var "a")),
  -- printLn (int) (arg 5),
  -- printLn (int) (extVar "Lib" "var"),
  -- printLn (int) (self),
  -- printLn (int) (objVarSelf "thisOne"),
  -- printLn (int) (objVar (var "outer") (var "inner")),
  -- printLn (int) (extFuncApp "myLib" "myFunc" [(var "arg1"), (var "arg2")]),
  -- printLn (int) (stateObj bool [(var "arg1"), (var "arg2")]),
  -- printLn (int) (listStateObj bool [(var "arg1"), (var "arg2")])]

helloIfExists :: (RenderSym repr) => repr (Statement repr)
helloIfExists = ifExists (var "boringList") (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

helloSwitch :: (RenderSym repr) => repr (Statement repr)
helloSwitch = switch (var "a") [((litInt 5), (oneLiner ("b" &.= (litInt 10)))), 
  ((litInt 0), (oneLiner ("b" &.= (litInt 5))))]
  (oneLiner ("b" &.= (litInt 0)))

helloForLoop :: (RenderSym repr) => repr (Statement repr)
helloForLoop = forRange "i" (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn (int) (var "i")))

helloWhileLoop :: (RenderSym repr) => repr (Statement repr)
helloWhileLoop = while (var "a" ?< (litInt 13)) (bodyStatements [printStrLn "Hello", ((&.++) "a")]) 

helloForEachLoop :: (RenderSym repr) => repr (Statement repr)
helloForEachLoop = forEach "num" (float) (listVar "myOtherList" (float)) 
  (oneLiner (printLn (float) (funcApp "doubleAndAdd" [(iterVar "num"), (litFloat 1.0)])))

helloTryCatch :: (RenderSym repr) => repr (Statement repr)
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))

doubleAndAdd :: (RenderSym repr) => repr (Method repr)
doubleAndAdd = function "doubleAndAdd" public static (mState float) [(stateParam "num1" float), (stateParam "num2" float)]
  (bodyStatements [(varDec "doubledSum" float), ("doubledSum" &.= (((litFloat 2.0) #* (var "num1")) #+ ((litFloat 2.0) #* (var "num2")))),
    (returnVar "doubledSum")])