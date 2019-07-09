{-# LANGUAGE PostfixOperators #-}

module Test.HelloWorld (helloWorld) where

import Language.Drasil.Code (
  PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..),  ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), Selector(..), FunctionSym(..), SelectorFunction(..), 
  MethodSym(..), ModuleSym(..), BlockCommentSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan)
import Test.Helper (helper)

helloWorld :: (PackageSym repr) => repr (Package repr)
helloWorld = packMods "HelloWorld" [commentedMod description $ 
  fileDoc (buildModule "HelloWorld" ["Helper"] [helloWorldMain] []), helper]

description :: (RenderSym repr) => repr (BlockComment repr)
description = blockComment ["This module tests various GOOL functions",
                            "It should run without errors."]

helloWorldMain :: (RenderSym repr) => repr (Method repr)
helloWorldMain = mainMethod "HelloWorld" (body [ helloInitVariables, 
    helloListSlice,
    block [ifCond [(var "b" int ?>= litInt 6, bodyStatements [varDecDef "dummy" string (litString "dummy")]),
      (var "b" int ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]])

helloInitVariables :: (RenderSym repr) => repr (Block repr)
helloInitVariables = block [comment "Initializing variables",
  varDec $ var "a" int, 
  varDecDef "b" int (litInt 5),
  listDecDef "myOtherList" (listType static_ float) [litFloat 1.0, litFloat 1.5],
  varDecDef "oneIndex" int (indexOf (var "myOtherList" (listType static_ float)) (litFloat 1.0)),
  printLn (var "oneIndex" int),
  var "a" int &= listSizeAccess (var "myOtherList" (listType static_ float)),
  valState (objAccess (var "myOtherList" (listType static_ float)) (listAdd 
    (var "myOtherList" (listType static_ float)) (litInt 2) (litFloat 2.0))),
  valState (objAccess (var "myOtherList" (listType static_ float)) (listAppend (litFloat 2.5))),
  varDec $ var "e" float,
  var "e" int &= objAccess (var "myOtherList" (listType static_ float)) (listAccess float (litInt 1)),
  valState (objAccess (var "myOtherList" (listType static_ float)) (listSet (litInt 1) (litFloat 17.4))),
  listDec "myName" 7 (listType static_ string),
  stringSplit ' ' (var "myName" (listType static_ string)) (litString "Brooks Mac"),
  printLn (var "myName" (listType static_ string)),
  listDecDef "boringList" (listType dynamic_ bool) [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (var "boringList" (listType dynamic_ bool)),
  listDec "mySlicedList" 2 $ listType static_ float]

helloListSlice :: (RenderSym repr) => repr (Block repr)
helloListSlice = listSlice (listType static_ float) (var "mySlicedList" (listType static_ float)) (var "myOtherList" (listType static_ float)) (Just (litInt 1)) (Just (litInt 3)) Nothing

helloIfBody :: (RenderSym repr) => repr (Body repr)
helloIfBody = addComments "If body" (body [
  block [
    varDec $ var "c" int,
    varDec $ var "d" int,
    assign (var "a" int) (litInt 5),
    var "b" int &= (var "a" int #+ litInt 2),
    var "c" int &= (var "b" int #+ litInt 3),
    var "d" int &= var "b" int,
    var "d" int &-= var "a" int,
    var "c" int &-= var "d" int,
    var "b" int &+= litInt 17,
    var "c" int &+= litInt 17,
    (&++) (var "a" int),
    (&++) (var "d" int),
    (&~-) (var "c" int),
    (&~-) (var "b" int),

    listDec "myList" 5 (listType static_ int),
    objDecDef "myObj" char (litChar 'o'),
    constDecDef "myConst" string (litString "Imconstant"),

    printLn (var "a" int),
    printLn (var "b" int),
    printLn (var "c" int),
    printLn (var "d" int),
    printLn (var "myOtherList" (listType static_ float)),
    printLn (var "mySlicedList" (listType static_ float)),
    
    printStrLn "Type an int",
    getIntInput (var "d" int),
    printStrLn "Type another",
    discardInput],
  
  block [
    printLn (litString " too"),
    printStr "boo",
    print litTrue,
    print (litInt 0),
    print (litChar 'c'),
    printLn (litTrue ?!),
    printLn (litInt 1 #~),
    printLn (litFloat 4.0 #/^),
    printLn (litInt (-4) #|),
    printLn (log (litFloat 2.0)),
    multi [printLn (ln (litFloat 2.0)),
    printLn (exp (litFloat 2.0 #~)),
    printLn (sin (litFloat 2.0)),
    printLn (cos (litFloat 2.0)),
    printLn (tan (litFloat 2.0))],
    printLn (tan (litFloat 2.0)),
    printLn (litTrue ?&& litFalse),
    printLn (litTrue ?|| litFalse),
    printLn (litTrue ?&& (litFalse ?!)),
    printLn ((litTrue ?&& litTrue) ?!),
    printLn (litInt 6 #+ litInt 2),
    printLn (litInt 6 #- litInt 2),
    printLn (litInt 6 #* litInt 2),
    printLn (litInt 6 #/ litInt 2),
    printLn (litInt 6 #% litInt 4),
    printLn (litInt 6 #^ litInt 2),
    printLn (litInt 6 #+ (litInt 2 #* litInt 3)),
    printLn (csc (litFloat 1.0)),
    printLn (sec (litFloat 1.0)),
    printLn (var "a" int),
    printLn (inlineIf litTrue (litInt 5) (litInt 0)),
    printLn (cot (litFloat 1.0))]])

helloElseBody :: (RenderSym repr) => repr (Body repr)
helloElseBody = bodyStatements [printLn (arg 5)]

helloIfExists :: (RenderSym repr) => repr (Statement repr)
helloIfExists = ifExists (var "boringList" (listType dynamic_ bool)) (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

helloSwitch :: (RenderSym repr) => repr (Statement repr)
helloSwitch = switch (var "a" int) [(litInt 5, oneLiner (var "b" int &= litInt 10)), 
  (litInt 0, oneLiner (var "b" int &= litInt 5))]
  (oneLiner (var "b" int &= litInt 0))

helloForLoop :: (RenderSym repr) => repr (Statement repr)
helloForLoop = forRange "i" (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn (var "i" int)))

helloWhileLoop :: (RenderSym repr) => repr (Statement repr)
helloWhileLoop = while (var "a" int ?< litInt 13) (bodyStatements [printStrLn "Hello", (&++) (var "a" int)]) 

helloForEachLoop :: (RenderSym repr) => repr (Statement repr)
helloForEachLoop = forEach "num" float (listVar "myOtherList" static_ float) 
  (oneLiner (printLn (extFuncApp "Helper" "doubleAndAdd" float [iterVar "num" float, litFloat 1.0])))

helloTryCatch :: (RenderSym repr) => repr (Statement repr)
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))
