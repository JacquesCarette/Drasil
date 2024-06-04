{-# LANGUAGE PostfixOperators #-}
-- | Tests basic GOOL functions. It *might* run without errors.
module SuperSimple (superSimple) where

import GOOL.Drasil (GSProgram, MSBody, MSBlock, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), ControlStatement(..), 
  CommandLineArgs(..), BlockSym(..), TypeSym(..), StatementSym(..), 
  Comparison(..), (&=), DeclStatement(..), IOStatement(..), StringStatement(..),
  CommentStatement(..), VariableSym(..), Literal(..), VariableValue(..), 
  List(..), MethodSym(..), ModuleSym(..), VSType, SVariable, ValueExpression, 
  SValue, listSlice, bodyStatements, extNewObj)
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import SimpleClass (simpleClass, simpleName)

-- | Creates the SuperSimple program and necessary files.
superSimple :: (OOProg r) => GSProgram r
superSimple = prog "SuperSimple" "" [docMod description
  ["Brooks MacLachlan"] "" $ fileDoc (buildModule "SuperSimple" []
  [superSimpleMain] []), simpleClass]

-- | Description of program.
description :: String
description = "Tests basic GOOL functions. It *might* run without errors."

-- | Main function. Initializes variables and combines all the helper functions defined below.
superSimpleMain :: (OOProg r) => SMethod r
superSimpleMain = mainFunction (body [ helloInitVariables,
    helloListSlice, objTest{-,
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

-- | Test object functionality
simpleClassType :: (TypeSym r) => VSType r
simpleClassType = obj simpleName

s :: (VariableSym r) => SVariable r
s = var "s" simpleClassType

newSimpleClass :: (ValueExpression r) => SValue r
newSimpleClass = extNewObj simpleName simpleClassType []

objTest :: (OOProg r) => MSBlock r
objTest = block [
  s &= newSimpleClass]

-- | Print the 5th given argument.
helloElseBody :: (OOProg r) => MSBody r
helloElseBody = bodyStatements [printLn (arg 5)]