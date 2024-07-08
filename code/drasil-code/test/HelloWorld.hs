{-# LANGUAGE PostfixOperators #-}
-- | GOOL test program for various OO program functionality.
-- Should run print statements, basic loops, math, and create a helper module without errors.
module HelloWorld (helloWorld) where

import GOOL.Drasil (GSProgram, MSBody, MSBlock, MSStatement, SMethod, SVariable,
  OOProg, ProgramSym(..), FileSym(..), BodySym(..), bodyStatements, oneLiner,
  BlockSym(..), listSlice, TypeSym(..), StatementSym(..), AssignStatement(..), 
  (&=), DeclStatement(..), IOStatement(..), StringStatement(..),
  CommentStatement(..), ControlStatement(..), VariableSym(..), ScopeSym(..),
  listVar, Literal(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), extFuncApp, List(..), MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import Helper (helper)

-- | Creates the HelloWorld program and necessary files.
helloWorld :: (OOProg r) => GSProgram r
helloWorld = prog "HelloWorld" "" [docMod description
  ["Brooks MacLachlan"] "" $ fileDoc (buildModule "HelloWorld" []
  [helloWorldMain] []), helper]

-- | Description of program.
description :: String
description = "Tests various GOOL functions. It should run without errors."

-- | Variable for a list of doubles
myOtherList :: (OOProg r) => SVariable r
myOtherList = var "myOtherList" (listType double) local

-- | Main function. Initializes variables and combines all the helper functions defined below.
helloWorldMain :: (OOProg r) => SMethod r
helloWorldMain = mainFunction (body ([ helloInitVariables] ++ listSliceTests ++
    [block [printLn $ litString "", ifCond [
      (valueOf (var "b" int local) ?>= litInt 6, bodyStatements [
        varDecDef (var "dummy" string local) (litString "dummy")]),
      (valueOf (var "b" int local) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]]))

-- | Initialize variables used in the generated program.
helloInitVariables :: (OOProg r) => MSBlock r
helloInitVariables = block [comment "Initializing variables",
  varDec $ var "a" int local,
  varDecDef (var "b" int local) (litInt 5),
  listDecDef myOtherList [litDouble 1.0,
    litDouble 1.5],
  varDecDef (var "oneIndex" int local) (indexOf (valueOf myOtherList) (litDouble 1.0)),
  printLn (valueOf $ var "oneIndex" int local),
  var "a" int local &= listSize 
    (valueOf myOtherList),
  valStmt (listAdd (valueOf myOtherList)
    (litInt 2) (litDouble 2.0)),
  valStmt (listAppend (valueOf $ var "myOtherList" (listType double))
    (litDouble 2.5)),
  varDec $ var "e" double local,
  var "e" int local &= listAccess (valueOf myOtherList) (litInt 1),
  valStmt (listSet (valueOf myOtherList)
    (litInt 1) (litDouble 17.4)),
  listDec 7 (var "myName" (listType string) local),
  stringSplit ' ' (var "myName" (listType string) local) (litString "Brooks Mac"),
  printLn (valueOf $ var "myName" (listType string) local),
  listDecDef (var "boringList" (listType bool) local)
    [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (valueOf $ var "boringList" (listType bool) local)]

mySlicedList, mySlicedList2, mySlicedList3, mySlicedList4, mySlicedList5,
  mySlicedList6, mySlicedList7, mySlicedList8, mySlicedList9, 
  mySlicedList10, mySlicedList11 :: (OOProg r) => SVariable r
mySlicedList = var "mySlicedList" (listType double) local
mySlicedList2 = var "mySlicedList2" (listType double) local
mySlicedList3 = var "mySlicedList3" (listType double) local
mySlicedList4 = var "mySlicedList4" (listType double) local
mySlicedList5 = var "mySlicedList5" (listType double) local
mySlicedList6 = var "mySlicedList6" (listType double) local
mySlicedList7 = var "mySlicedList7" (listType double) local
mySlicedList8 = var "mySlicedList8" (listType double) local
mySlicedList9 = var "mySlicedList9" (listType double) local
mySlicedList10 = var "mySlicedList10" (listType double) local
mySlicedList11 = var "mySlicedList11" (listType double) local

listSliceTests :: (OOProg r) => [MSBlock r]
listSliceTests = [

  -- | Declare variables for list slices
  block [
    comment "List slicing tests",
    comment "Create variables for list slices",
    listDec 2 mySlicedList,
    listDec 2 mySlicedList2,
    listDec 3 mySlicedList3,
    listDec 0 mySlicedList4,
    listDec 2 mySlicedList5,
    listDec 2 mySlicedList6,
    listDec 4 mySlicedList7,
    listDec 3 mySlicedList8,
    listDec 2 mySlicedList9,
    listDec 3 mySlicedList10,
    listDec 0 mySlicedList11],

  -- | Initialize and assign any variables necessary for list slices
  block [
    comment "Create some variables for later tests",
    varDecDef (var "x" int local) (litInt 3),
    varDecDef (var "y" int local) (litInt 1),
    varDecDef (var "z" int) (litInt (-1))],

  -- | Initialize and assign a value to a new variable @mySlicedList@.
  --   Both bounds are set, end > start, with step defaulting to 1
  listSlice mySlicedList
    (valueOf myOtherList) (Just (litInt 1))
    (Just (litInt 3)) Nothing,

  -- List slicing with step > 1
  listSlice mySlicedList2
    (valueOf myOtherList) (Just (litInt 1))
    (Just (litInt 4)) (Just (litInt 2)),

  -- List slicing with positive step, no end given
  listSlice mySlicedList3
    (valueOf myOtherList) (Just (litInt 1))
    Nothing Nothing,

  -- | List slicing with start > end but positive step
  listSlice mySlicedList4
    (valueOf myOtherList) (Just (litInt 3))
    (Just (litInt 1)) Nothing,

  -- | List slicing with start > end and negative step
  listSlice mySlicedList5
    (valueOf myOtherList) (Just (litInt 3))
    (Just (litInt 1)) (Just (litInt (-1))),

  -- | List slicing with no start given and negative step
  listSlice mySlicedList6
    (valueOf myOtherList) Nothing
    (Just (litInt 1)) (Just (litInt (-1))),

  -- | List slicing with no end given and negative step
  listSlice mySlicedList7
    (valueOf myOtherList) (Just (litInt 3))
    Nothing (Just (litInt (-1))),

  -- | List slicing where the step is a variable with negative value
  listSlice mySlicedList8
    (valueOf myOtherList) (Just (litInt 3))
    (Just (litInt 0)) (Just (valueOf (var "z" int))),

  -- | List slicing where the bounds are variables with start > end, and step is a variable < 0
  listSlice mySlicedList9
    (valueOf myOtherList) (Just (valueOf (var "x" int)))
    (Just (valueOf (var "y" int))) (Just (valueOf (var "z" int))),

  -- | List slicing where end isn't given and step is a variable < 0
  listSlice mySlicedList10
    (valueOf myOtherList) (Just (litInt 2))
    Nothing (Just (valueOf (var "z" int))),

  -- | List slicing where end > beg, but step is a variable < 0
  listSlice mySlicedList11
    (valueOf myOtherList) (Just (valueOf (var "y" int)))
    (Just (valueOf (var "x" int))) (Just (valueOf (var "z" int))),

  -- | Print results of list slicing tests
  block [
    comment "Print results of list slicing tests",
    printLn $ litString "",
    printLn $ litString "List slicing:",
    print $ litString "myOtherList: ",
    printLn $ valueOf myOtherList,
    print $ litString "mySlicedList: ",
    printLn $ valueOf mySlicedList,
    print $ litString "mySlicedList2: ",
    printLn $ valueOf mySlicedList2,
    print $ litString "mySlicedList3: ",
    printLn $ valueOf mySlicedList3,
    print $ litString "mySlicedList4: ",
    printLn $ valueOf mySlicedList4,
    print $ litString "mySlicedList5: ",
    printLn $ valueOf mySlicedList5,
    print $ litString "mySlicedList6: ",
    printLn $ valueOf mySlicedList6,
    print $ litString "mySlicedList7: ",
    printLn $ valueOf mySlicedList7,
    print $ litString "mySlicedList8: ",
    printLn $ valueOf mySlicedList8,
    print $ litString "mySlicedList9: ",
    printLn $ valueOf mySlicedList9,
    print $ litString "mySlicedList10: ",
    printLn $ valueOf mySlicedList10,
    print $ litString "mySlicedList11: ",
    printLn $ valueOf mySlicedList11]]

-- | Create an If statement.
{-# ANN module "HLint: ignore Evaluate" #-}
helloIfBody :: (OOProg r) => MSBody r
helloIfBody = addComments "If body" (body [
  block [
    varDec $ var "c" int local,
    varDec $ var "d" int local,
    assign (var "a" int local) (litInt 5),
    var "b" int local &= (valueOf (var "a" int local) #+ litInt 2),
    var "c" int local &= (valueOf (var "b" int local) #+ litInt 3),
    var "d" int local &= valueOf (var "b" int local),
    var "d" int local &-= valueOf (var "a" int local),
    var "c" int local &-= valueOf (var "d" int local),
    var "b" int local &+= litInt 17,
    var "c" int local &+= litInt 17,
    (&++) (var "a" int local),
    (&++) (var "d" int local),
    (&--) (var "c" int local),
    (&--) (var "b" int local),

    listDec 5 (var "myList" (listType int) local),
    objDecDef (var "myObj" char local) (litChar 'o'),
    constDecDef (constant "myConst" string local) (litString "Imconstant"),

    printLn (valueOf $ var "a" int local),
    printLn (valueOf $ var "b" int local),
    printLn (valueOf $ var "c" int local),
    printLn (valueOf $ var "d" int local),
    printLn (valueOf myOtherList),
    printLn (valueOf $ var "mySlicedList" (listType double) local),

    printStrLn "Type an int",
    getInput (var "d" int local),
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
    printLn (litDouble 4.0 #/^),
    printLn (litInt (-4) #|),
    printLn (log (litDouble 2.0)),
    multi [printLn (ln (litDouble 2.0)),
    printLn (exp (litDouble 2.0 #~)),
    printLn (sin (litDouble 2.0)),
    printLn (cos (litDouble 2.0)),
    printLn (tan (litDouble 2.0))],
    printLn (tan (litDouble 2.0)),
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
    printLn (csc (litDouble 1.0)),
    printLn (sec (litDouble 1.0)),
    printLn (valueOf $ var "a" int local),
    printLn (inlineIf litTrue (litInt 5) (litInt 0)),
    printLn (cot (litDouble 1.0))]])

-- | Print the 5th given argument.
helloElseBody :: (OOProg r) => MSBody r
helloElseBody = bodyStatements [printLn (arg 5)]

-- | If-else statement checking if a list is empty.
helloIfExists :: (OOProg r) => MSStatement r
helloIfExists = ifExists (valueOf $ var "boringList" (listType bool) local)
  (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

-- | Creates a switch statement.
helloSwitch :: (OOProg r) => MSStatement r
helloSwitch = switch (valueOf $ var "a" int local) [(litInt 5, oneLiner (var "b" int local &= litInt 10)),
  (litInt 0, oneLiner (var "b" int local &= litInt 5))]
  (oneLiner (var "b" int local &= litInt 0))

-- | Creates a for loop.
helloForLoop :: (OOProg r) => MSStatement r
helloForLoop = forRange i (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn
  (valueOf i)))
  where i = var "i" int local

-- | Creates a while loop.
helloWhileLoop :: (OOProg r) => MSStatement r
helloWhileLoop = while (valueOf (var "a" int local) ?< litInt 13) (bodyStatements
  [printStrLn "Hello", (&++) (var "a" int local)])

-- | Creates a for-each loop.
helloForEachLoop :: (OOProg r) => MSStatement r
helloForEachLoop = forEach i (valueOf $ listVar "myOtherList" double)
  (oneLiner (printLn (extFuncApp "Helper" "doubleAndAdd" double [valueOf i,
  litDouble 1.0])))
  where i = var "num" double local

-- | Creates a try statement to catch an intentional error.
helloTryCatch :: (OOProg r) => MSStatement r
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))
