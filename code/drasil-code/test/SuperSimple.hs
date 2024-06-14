{-# LANGUAGE PostfixOperators #-}
-- | Tests basic GOOL functions. It *might* run without errors.
module SuperSimple (superSimple) where

import GOOL.Drasil (GSProgram, MSBody, MSBlock, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), ControlStatement(..), 
  CommandLineArgs(..), BlockSym(..), TypeSym(..), StatementSym(..), 
  Comparison(..), (&=), DeclStatement(..), IOStatement(..), StringStatement(..),
  CommentStatement(..), VariableSym(..), Literal(..), VariableValue(..), 
  List(..), MethodSym(..), ModuleSym(..), VSType, SVariable, ValueExpression, 
  SValue, listSlice, bodyStatements, extFuncApp, extNewObj, objVar,
  objMethodCall, objMethodCallNoParams, stateVar, ScopeSym(..), PermanenceSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import SimpleClass (simpleClass, simpleClassName, simpleClassType)
import SimpleLib (simpleLib, doubleAndAdd)

-- | Creates the SuperSimple program and necessary files.
superSimple :: (OOProg r) => GSProgram r
superSimple = prog "SuperSimple" "" [docMod description
  ["Brooks MacLachlan"] "" $ fileDoc (buildModule "SuperSimple" []
  [superSimpleMain] [])]

-- | Description of program.
description :: String
description = "Tests basic GOOL functions. It *might* run without errors."

-- | Main function. Initializes variables and combines all the helper functions defined below.
superSimpleMain :: (OOProg r) => SMethod r
superSimpleMain = mainFunction (body [ helloInitVariables{-, objTest, objTest2,
    block [ifCond [(valueOf (var "b" int) ?>= litInt 6, bodyStatements [varDecDef (var "dummy" string) (litString "dummy")]),
      (valueOf (var "b" int) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]-}])

-- | Initialize variables used in the generated program.
helloInitVariables :: (OOProg r) => MSBlock r
helloInitVariables = block [comment "Initializing variables",
  varDec $ var "a" int,
  varDecDef (var "b" int) (litInt 5),
  listDecDef (var "myOtherList" (listType double)) [litDouble 1.0,
    litDouble 1.5],
  varDecDef (var "oneIndex" int) (indexOf (valueOf $ var "myOtherList"
    (listType double)) (litDouble 1.0)),
  printLn (valueOf $ var "oneIndex" int),
  var "a" int &= listSize (valueOf $ var "myOtherList" (listType double)),
  valStmt (listAdd (valueOf $ var "myOtherList" (listType double))
    (litInt 2) (litDouble 2.0)),
  valStmt (listAppend (valueOf $ var "myOtherList" (listType double))
    (litDouble 2.5)),
  varDec $ var "e" double,
  var "e" int &= listAccess (valueOf $ var "myOtherList"
    (listType double)) (litInt 1),
  valStmt (listSet (valueOf $ var "myOtherList" (listType double))
    (litInt 1) (litDouble 17.4)),
  listDec 7 (var "myName" (listType string)),
  stringSplit ' ' (var "myName" (listType string)) (litString "Brooks Mac"),
  printLn (valueOf $ var "myName" (listType string)),
  listDecDef (var "boringList" (listType bool))
    [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (valueOf $ var "boringList" (listType bool)),
  listDec 2 $ var "mySlicedList" (listType double)]

-- | Initialize and assign a value to a new variable @mySlicedList@.
helloListSlice :: (OOProg r) => MSBlock r
helloListSlice = listSlice (var "mySlicedList" (listType double))
  (valueOf $ var "myOtherList" (listType double)) (Just (litInt 9))
  Nothing (Just (litInt (-1)))

s, s2, x :: (VariableSym r) => SVariable r
s = var "s" simpleClassType
s2 = var "s2" simpleClassType
x = var "x" int

newSimpleClass :: (ValueExpression r) => SValue r
newSimpleClass = extNewObj simpleClassName simpleClassType []

objTest :: (OOProg r) => MSBlock r
objTest = block [
  varDecDef s newSimpleClass,
  printLn $ valueOf s,
  printLn $ objMethodCallNoParams int (valueOf s) "getX",
  valStmt $ objMethodCall void (valueOf s) "setX" [litInt 2],
  printLn $ objMethodCallNoParams int (valueOf s) "getX",
  valStmt $ objMethodCall void (valueOf s) "resetXIfTrue" [litTrue],
  printLn $ objMethodCallNoParams int (valueOf s) "getX"]

objTest2 :: (OOProg r) => MSBlock r
objTest2 = block [
  varDecDef s2 $ extFuncApp "SimpleData" "makeSimpleData" simpleClassType 
    [litInt 4],
  printLn $ objMethodCallNoParams int (valueOf s2) "getX",
  valStmt $ extFuncApp "SimpleData" "updateSimpleData" void [valueOf s2, litInt 8],
  printLn $ objMethodCallNoParams int (valueOf s2) "getX"]

-- | Print the 5th given argument.
helloElseBody :: (OOProg r) => MSBody r
helloElseBody = bodyStatements [printLn (arg 5)]