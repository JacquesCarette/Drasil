{-# LANGUAGE PostfixOperators #-}
-- | GOOL test program for various OO program functionality.
-- Should run print statements, basic loops, math, and create a helper module without errors.
module GOOL.HelloWorld (helloWorld) where

import Drasil.GOOL (GSProgram, MSBody, MSBlock, MSStatement, SMethod, SVariable,
  OOProg, ProgramSym(..), FileSym(..), BodySym(..), bodyStatements, oneLiner,
  BlockSym(..), listSlice, TypeSym(..), StatementSym(..), AssignStatement(..), 
  (&=), DeclStatement(..), IOStatement(..), StringStatement(..),
  CommentStatement(..), ControlStatement(..), mainVar, locVar, constant,
  ScopeSym(..), Literal(..), VariableValue(..), CommandLineArgs(..),
  NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), extFuncApp, List(..), Set(..), MethodSym(..), ModuleSym(..),
  OODeclStatement(objDecDef))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import GOOL.Helper (helper)

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
myOtherList = mainVar "myOtherList" (listType double)

-- | Main function. Initializes variables and combines all the helper functions defined below.
helloWorldMain :: (OOProg r) => SMethod r
helloWorldMain = mainFunction (body ([ helloInitVariables] ++ listSliceTests ++
    [block [printLn $ litString "", ifCond [
      (valueOf (mainVar "b" int) ?>= litInt 6, bodyStatements [
        varDecDef (mainVar "dummy" string) (litString "dummy")]),
      (valueOf (mainVar "b" int) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]]))

-- | Initialize variables used in the generated program.
helloInitVariables :: (OOProg r) => MSBlock r
helloInitVariables = block [comment "Initializing variables",
  varDec $ mainVar "a" int,
  varDecDef (mainVar "b" int) (litInt 5),
  listDecDef myOtherList [litDouble 1.0,
    litDouble 1.5],
  varDecDef (mainVar "oneIndex" int) (indexOf (valueOf myOtherList) (litDouble 1.0)),
  printLn (valueOf $ mainVar "oneIndex" int),
  mainVar "a" int &= listSize (valueOf myOtherList),
  valStmt (listAdd (valueOf myOtherList)
    (litInt 2) (litDouble 2.0)),
  valStmt (listAppend (valueOf myOtherList)
    (litDouble 2.5)),
  varDec $ mainVar "e" double,
  mainVar "e" int &= listAccess (valueOf myOtherList) (litInt 1),
  valStmt (listSet (valueOf myOtherList)
    (litInt 1) (litDouble 17.4)),
  listDec 7 (mainVar "myName" (listType string)),
  stringSplit ' ' (mainVar "myName" (listType string)) (litString "Brooks Mac"),
  printLn (valueOf $ mainVar "myName" (listType string)),
  listDecDef (mainVar "boringList" (listType bool))
    [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (valueOf $ mainVar "boringList" (listType bool))]

mySlicedList, mySlicedList2, mySlicedList3, mySlicedList4, mySlicedList5,
  mySlicedList6, mySlicedList7, mySlicedList8, mySlicedList9, 
  mySlicedList10, mySlicedList11 :: (OOProg r) => SVariable r
mySlicedList = mainVar "mySlicedList" (listType double)
mySlicedList2 = mainVar "mySlicedList2" (listType double)
mySlicedList3 = mainVar "mySlicedList3" (listType double)
mySlicedList4 = mainVar "mySlicedList4" (listType double)
mySlicedList5 = mainVar "mySlicedList5" (listType double)
mySlicedList6 = mainVar "mySlicedList6" (listType double)
mySlicedList7 = mainVar "mySlicedList7" (listType double)
mySlicedList8 = mainVar "mySlicedList8" (listType double)
mySlicedList9 = mainVar "mySlicedList9" (listType double)
mySlicedList10 = mainVar "mySlicedList10" (listType double)
mySlicedList11 = mainVar "mySlicedList11" (listType double)

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
    varDecDef (mainVar "x" int) (litInt 3),
    varDecDef (mainVar "y" int) (litInt 1),
    varDecDef (mainVar "z" int) (litInt (-1))],

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
    (Just (litInt 0)) (Just (valueOf (mainVar "z" int))),

  -- | List slicing where the bounds are variables with start > end, and step is a variable < 0
  listSlice mySlicedList9
    (valueOf myOtherList) (Just (valueOf (mainVar "x" int)))
    (Just (valueOf (mainVar "y" int))) (Just (valueOf (mainVar "z" int))),

  -- | List slicing where end isn't given and step is a variable < 0
  listSlice mySlicedList10
    (valueOf myOtherList) (Just (litInt 2))
    Nothing (Just (valueOf (mainVar "z" int))),

  -- | Do it again, to make sure a unique variable name for endIdx is being generated
  listSlice mySlicedList10
  (valueOf myOtherList) (Just (litInt 2))
  Nothing (Just (valueOf (mainVar "z" int))),

  -- | List slicing where end > beg, but step is a variable < 0
  listSlice mySlicedList11
    (valueOf myOtherList) (Just (valueOf (mainVar "y" int)))
    (Just (valueOf (mainVar "x" int))) (Just (valueOf (mainVar "z" int))),

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
    varDec $ mainVar "c" int,
    varDec $ mainVar "d" int,
    assign (mainVar "a" int) (litInt 5),
    mainVar "b" int &= (valueOf (mainVar "a" int) #+ litInt 2),
    mainVar "c" int &= (valueOf (mainVar "b" int) #+ litInt 3),
    mainVar "d" int &= valueOf (mainVar "b" int),
    mainVar "d" int &-= valueOf (mainVar "a" int),
    mainVar "c" int &-= valueOf (mainVar "d" int),
    mainVar "b" int &+= litInt 17,
    mainVar "c" int &+= litInt 17,
    (&++) (mainVar "a" int),
    (&++) (mainVar "d" int),
    (&--) (mainVar "c" int),
    (&--) (mainVar "b" int),

    listDec 5 (mainVar "myList" (listType int)),
    objDecDef (mainVar "myObj" char) (litChar 'o'),
    constDecDef (constant "myConst" string mainFn) (litString "Imconstant"),

    printLn (valueOf $ constant "myConst" string mainFn),
    printLn (valueOf $ mainVar "a" int),
    printLn (valueOf $ mainVar "b" int),
    printLn (valueOf $ mainVar "c" int),
    printLn (valueOf $ mainVar "d" int),
    printLn (valueOf myOtherList),
    printLn (valueOf mySlicedList),

    printStrLn "Type an int",
    getInput (mainVar "d" int),
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
    printLn (valueOf $ mainVar "a" int),
    printLn (inlineIf litTrue (litInt 5) (litInt 0)),
    printLn (cot (litDouble 1.0))]])

-- | Print the 5th given argument.
helloElseBody :: (OOProg r) => MSBody r
helloElseBody = bodyStatements [printLn (arg 5)]

-- | If-else statement checking if a list is empty.
helloIfExists :: (OOProg r) => MSStatement r
helloIfExists = ifExists (valueOf $ mainVar "boringList" (listType bool))
  (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

-- | Creates a switch statement.
helloSwitch :: (OOProg r) => MSStatement r
helloSwitch = switch (valueOf $ mainVar "a" int) [(litInt 5, oneLiner (mainVar "b" int &= litInt 10)),
  (litInt 0, oneLiner (mainVar "b" int &= litInt 5))]
  (oneLiner (mainVar "b" int &= litInt 0))

-- | Creates a for loop.
helloForLoop :: (OOProg r) => MSStatement r
helloForLoop = forRange i (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn
  (valueOf i)))
  where i = locVar "i" int

-- | Creates a while loop.
helloWhileLoop :: (OOProg r) => MSStatement r
helloWhileLoop = while (valueOf (mainVar "a" int) ?< litInt 13) (bodyStatements
  [printStrLn "Hello", (&++) (mainVar "a" int)])

-- | Creates a for-each loop.
helloForEachLoop :: (OOProg r) => MSStatement r
helloForEachLoop = forEach i (valueOf myOtherList)
  (oneLiner (printLn (extFuncApp "Helper" "doubleAndAdd" double [valueOf i,
  litDouble 1.0])))
  where i = locVar "num" double

-- | Creates a try statement to catch an intentional error.
helloTryCatch :: (OOProg r) => MSStatement r
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))
