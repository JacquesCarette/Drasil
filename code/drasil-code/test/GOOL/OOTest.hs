-- | GOOL OOTest module. Various tests of general object functionality.
module GOOL.OOTest (ooTest) where

import Drasil.GOOL (OOProg, GSProgram, SMethod, ProgramSym(..), FileSym (..),
  ModuleSym (..), MethodSym (..), BodySym (..), IOStatement (..), Literal (..),
  SClass, ClassSym (..), pubDVar, VariableSym (..), TypeSym (..),
  ParameterSym (..), VariableValue (..), StateVarSym (..), VisibilitySym (..),
  PermanenceSym (..), initializer, pubMethod, NumericExpression (..),
  ControlStatement (..), OOVariableSym (..), OOMethodSym (..), OOTypeSym (..),
  staticVar, DeclStatement (..), ScopeSym (..), newObj, objMethodCall, funcApp)
import Drasil.Metadata (watermark)
import Drasil.GProc (BlockSym(..))

ooTest :: (OOProg r) => GSProgram r
ooTest = prog "OOTest" "Tests various aspects of general object functionality"
  [docMod "Tests various aspects of general object functionality" watermark
  ["Brandon Bosman"] "Apr. 24, 2026" (fileDoc (buildModule "OOTest" [] [ooTestMain] [testClass]))]

ooTestMain :: (OOProg r) => SMethod r
ooTestMain = mainFunction (body [block [
    printLn $ valueOf $ classVar (obj "testClass") (staticVar "y" double),
    varDecDef (var "testObj" (obj "testClass")) mainFn (newObj (obj "testClass") [litInt 56]),
    printLn $ objMethodCall int (valueOf $ var "testObj" (obj "testClass")) "mulByX" [litInt 42],
    --Hack:
    printLn $ funcApp "testClass.square" double [litDouble 2.5]]
  ])

testClass :: (OOProg r) => SClass r
testClass = docClass testClassDesc (extraClass "testClass" Nothing
  [pubDVar $ var "x" int, stateVarDef public static  (var "y" double) (litDouble 17.4)]
  [testConstructor] [testMethod, testStaticMethod])

testClassDesc :: String
testClassDesc = "Basic class to test some stuff"

testConstructor :: (OOProg r) => SMethod r
testConstructor = initializer [param $ var "x" int] [(var "x" int, valueOf $ var "x" int)]

testMethod :: (OOProg r) => SMethod r
testMethod = pubMethod "mulByX" int [param $ var "num" int]
  (body [block [returnStmt $ valueOf (objVarSelf $ var "x" int) #* valueOf (var "num" int)]])

testStaticMethod :: (OOProg r) => SMethod r
testStaticMethod = method "square" public static double [param $ var "num" double]
  (body [block [returnStmt $ valueOf (var "num" double) #* valueOf (var "num" double)]])
