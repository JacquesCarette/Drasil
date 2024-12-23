{-# LANGUAGE PostfixOperators #-}
-- | GOOL test program for various OO program functionality.
-- Should run print statements, basic loops, math, and create a helper module without errors.
module HelloWorld (helloWorldOO, helloWorldProc) where

import Drasil.GOOL (MSBlock, SMethod, SVariable, SharedProg, OOProg,
  BodySym(..), BlockSym(..), VariableSym(..), Literal(..), VariableValue(..),
  TypeSym(..), ScopeSym(..), MethodSym(..), StatementSym(..),
  CommentStatement(..), DeclStatement(..), IOStatement(..),
  ControlStatement(..), StringStatement(..), List(..), at,
  NumericExpression(..), oneLiner, (&=), VisibilitySym(..), ParameterSym(..),
  funcApp, Comparison(..), ifNoElse, AssignStatement(..))
import qualified Drasil.GOOL as OO (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as GProc (GSProgram, ProgramSym(..), FileSym(..),
  ModuleSym(..))

import Prelude hiding (return,print,log,exp,sin,cos,tan,const,break)

-- | Creates the HelloWorld program and necessary files.
helloWorldOO :: (OOProg r) => OO.GSProgram r
helloWorldOO = OO.prog "HelloWorld" "" [OO.docMod description
  ["Brandon Bosman"] "" $ OO.fileDoc (OO.buildModule "HelloWorld" []
  [helloWorldMain, strToInt, sort] [])]

-- | Creates the HelloWorld program and necessary files.
helloWorldProc :: (ProcProg r) => GProc.GSProgram r
helloWorldProc = GProc.prog "HelloWorld" "" [GProc.docMod description
  ["Brandon Bosman"] "" $ GProc.fileDoc (GProc.buildModule "HelloWorld" []
  [helloWorldMain, strToInt, sort])]

-- | Description of program.
description :: String
description = "Advent of Code Day 1"

-- | Main function.
i, j :: (SharedProg r) => SVariable r
i = var "i" int
j = var "j" int

helloWorldMain :: (SharedProg r) => SMethod r
helloWorldMain = mainFunction (body [inputBlock, processBlock, distanceBlock])

-- Get input
filePath, fileHandle, fileContents :: (SharedProg r) => SVariable r
filePath = var "filePath" string
fileHandle = var "fileHandle" infile
fileContents = var "fileContents" (listType string)

inputBlock :: (SharedProg r) => MSBlock r
inputBlock = block [
    comment "Get input",
    varDecDef filePath global $ litString "../../../../drasil-code/test/input.txt",
    varDec fileHandle global,
    varDec fileContents global,
    openFileR fileHandle $ valueOf filePath,
    getFileInputAll (valueOf fileHandle) fileContents,
    closeFile (valueOf fileHandle)
  ]

-- Process input
list1, list2, line, splitLine :: (SharedProg r) => SVariable r
list1 = var "list1" (listType int)
list2 = var "list2" (listType int)
line = var "line" string
splitLine = var "splitLine" (listType string)

processBlock :: (SharedProg r) => MSBlock r
processBlock = block [
    comment "Process input",
    varDecDef list1 global (litArray int []),
    varDecDef list2 global (litArray int []),
    forEach line (valueOf fileContents) $ body [
      block [
        varDec splitLine local,
        stringSplit ' ' splitLine (valueOf line),
        valStmt $ listAppend (valueOf list1)
          (funcApp "strToInt" int [(valueOf splitLine) `at` (litInt 0)]),
        valStmt $ listAppend (valueOf list2)
          (funcApp "strToInt" int [(valueOf splitLine) `at` (litInt 3)])
      ]
    ],
    valStmt $ funcApp "selectionSort" (listType int) [valueOf list1],
    valStmt $ funcApp "selectionSort" (listType int) [valueOf list2]
  ]

-- Calculate distance and print out
distance, partDistance :: (SharedProg r) => SVariable r
distance = var "distance" int
partDistance = var "partDistance" int

distanceBlock :: (SharedProg r) => MSBlock r
distanceBlock = block [
    comment "Calculate distance and print out",
    varDecDef distance global (litInt 0),
    forRange i (litInt 0) (listSize (valueOf list1)) (litInt 1) $ body [block [
      varDecDef partDistance global $ ((valueOf list1) `at` (valueOf i)) #- ((valueOf list2) `at` (valueOf i)),
      ifNoElse [((valueOf partDistance) ?< (litInt 0),
        oneLiner $ partDistance &= ((valueOf partDistance) #~)
      )],
      distance &+= valueOf partDistance
    ]],
    print $ litString "Distance: ",
    printLn $ valueOf distance
  ]

-- Helper functions

-- strToInt
str, val, digit :: (SharedProg r) => SVariable r
str = var "str" (listType char)
val = var "val" int
digit = var "digit" int

strToInt :: (SharedProg r) => SMethod r
strToInt = docFunc "Converts a string to integer" ["String to be converted"]
  (Just "Integer value of the string") $
  function "strToInt" public int [param str] $ body [block[
    varDecDef val local (litInt 0),
    forRange i (litInt 0) (listSize (valueOf str)) (litInt 1) $
      body [block [
        varDec digit local,
        switch ((valueOf str) `at` (valueOf i)) [
          (litChar '0', oneLiner $ digit &= litInt 0),
          (litChar '1', oneLiner $ digit &= litInt 1),
          (litChar '2', oneLiner $ digit &= litInt 2),
          (litChar '3', oneLiner $ digit &= litInt 3),
          (litChar '4', oneLiner $ digit &= litInt 4),
          (litChar '5', oneLiner $ digit &= litInt 5),
          (litChar '6', oneLiner $ digit &= litInt 6),
          (litChar '7', oneLiner $ digit &= litInt 7),
          (litChar '8', oneLiner $ digit &= litInt 8),
          (litChar '9', oneLiner $ digit &= litInt 9),
          (litChar ' ', oneLiner continue),
          (litChar '\n', oneLiner continue)
        ] (oneLiner $ throw "Non-integer string given"),
        val &= (litInt 10) #* (valueOf val) #+ (valueOf digit)
      ]],
    returnStmt $ valueOf val
  ]]

-- Sort
xs, temp, maxIdx :: (SharedProg r) => SVariable r
xs = var "xs" (listType int)
temp = var "temp" int
maxIdx = var "maxIdx" int

sort :: (SharedProg r) => SMethod r
sort = docFunc "Sorts a list of integers" ["List to be sorted"] Nothing $
  function "selectionSort" public void [param xs] $ body [block [
    forRange i (litInt 0) (listSize (valueOf xs) #- (litInt 1)) (litInt 1) $
      body [block[
        varDecDef maxIdx local (valueOf i),
        forRange j (valueOf i #+ (litInt 1)) (listSize (valueOf xs)) (litInt 1) $
          body [block [
            ifNoElse [((valueOf xs) `at` (valueOf j) ?< (valueOf xs) `at` (valueOf maxIdx),
              oneLiner $ maxIdx &= valueOf j)]
            ]],
          varDecDef temp local ((valueOf xs) `at` (valueOf i)),
          valStmt $ listSet (valueOf xs) (valueOf i) ((valueOf xs) `at` (valueOf maxIdx)),
          valStmt $ listSet (valueOf xs) (valueOf maxIdx) (valueOf temp)
      ]]
  ]]