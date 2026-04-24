-- | GOOL OOTest module. Various tests of general object functionality.
module GOOL.OOTest (ooTest) where

import Drasil.GOOL (OOProg, GSProgram, SMethod, ProgramSym(..), FileSym (..),
  ModuleSym (..), MethodSym (..), BodySym (..), IOStatement (..), Literal (..),
  SClass, ClassSym (..), pubDVar, VariableSym (..), TypeSym (..),
  ParameterSym (..), VariableValue (..), StateVarSym (..), VisibilitySym (..),
  PermanenceSym (..), initializer, pubMethod, NumericExpression (..),
  ControlStatement (..), OOVariableSym (..), OOMethodSym (..), OOTypeSym (..),
  staticVar, DeclStatement (..), ScopeSym (..), newObj, objMethodCall, funcApp, VSType)
import Drasil.Metadata (watermark)
import Drasil.GProc (BlockSym(..))

ooTest :: (OOProg r) => GSProgram r
ooTest = prog "OOTest" progDesc
  [docMod progDesc watermark
  ["Brandon Bosman"] "Apr. 24, 2026" (fileDoc (buildModule "OOTest" [] [ooTestMain] [testClass]))]

progDesc :: String
progDesc = "Tests various aspects of general object functionality"

ooTestMain :: (OOProg r) => SMethod r
ooTestMain = mainFunction (body [block [
    printLn $ valueOf $ classVar testObjType (staticVar "y" double),
    varDecDef testObjVar mainFn (newObj testObjType [litInt 56]),
    printLn $ objMethodCall int testObjVar "mulByX" [litInt 42],
    --Hack:
    printLn $ funcApp "testClass.square" double [litDouble 2.5]]
  ])

varX :: (OOProg r) => SVariable
varX = var "x" int

testObjVar :: (OOProg r) => SVariable
testObjVar = var "testObj" testObjType

testObjType :: (OOProg r) => VSType r
testObjType = obj "testClass"

testClass :: (OOProg r) => SClass r
testClass = docClass testClassDesc (extraClass "testClass" Nothing
  [pubDVar varX, stateVarDef public static  (var "y" double) (litDouble 17.4)]
  [testConstructor] [testMethod, testStaticMethod])

testClassDesc :: String
testClassDesc = "Basic class to test some stuff"

testConstructor :: (OOProg r) => SMethod r
testConstructor = initializer [param varX] [(varX, valueOf varX)]

testMethod :: (OOProg r) => SMethod r
testMethod = pubMethod "mulByX" int [param $ var "num" int]
  (body [block [returnStmt $ valueOf (objVarSelf varX) #* valueOf (var "num" int)]])

testStaticMethod :: (OOProg r) => SMethod r
testStaticMethod = method "square" public static double [param $ var "num" double]
  (body [block [returnStmt $ valueOf (var "num" double) #* valueOf (var "num" double)]])
