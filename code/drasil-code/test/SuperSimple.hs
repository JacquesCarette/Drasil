{-# LANGUAGE PostfixOperators #-}
-- | GOOL test program for various OO program functionality.
-- Should run print statements, basic loops, math, and create a helper module without errors.
module SuperSimple (superSimple) where

import GOOL.Drasil (GSProgram, MSBody, MSBlock, MSStatement, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), bodyStatements, oneLiner,
  BlockSym(..), listSlice, TypeSym(..), StatementSym(..), AssignStatement(..), (&=),
  DeclStatement(..), IOStatement(..), StringStatement(..), CommentStatement(..), ControlStatement(..),
  VariableSym(..), listVar, Literal(..), VariableValue(..), CommandLineArgs(..), NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), extFuncApp, List(..),
  MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)

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
superSimpleMain = mainFunction (body [ helloInitVariables{-,
    helloListSlice,
    block [ifCond [(valueOf (var "b" int) ?>= litInt 6, bodyStatements [varDecDef (var "dummy" string) (litString "dummy")]),
      (valueOf (var "b" int) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]-}])

-- | Initialize variables used in the generated program.
helloInitVariables :: (OOProg r) => MSBlock r
helloInitVariables = block [comment "Initializing variables"]{-},
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
  listDec 2 $ var "mySlicedList" (listType double)]-}
