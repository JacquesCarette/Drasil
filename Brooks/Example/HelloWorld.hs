module Example.HelloWorld (helloWorld) where

import New (Declaration, StateVar, Scope, Label, Library,
  RenderSym(..), KeywordSym(..), PermanenceSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), BlockSym(..), ConditionalSym(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode [unJC helloWorld] ["HelloWorld"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

helloWorld :: (RenderSym repr) => repr (RenderFile repr)
helloWorld = fileDoc (
  ifCond [(litFalse, bodyStatements [(varDec "dummy" string)]),
  (litTrue, helloIfBody)] helloElseBody)

helloIfBody :: (RenderSym repr) => repr (Body repr)
helloIfBody = body [
  block [
    varDecDef "b" int (litInt 5),
    varDec "a" int,
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
    listDecDef "myOtherList" (floatListType static) [(litFloat 1.0), (litFloat 1.5)],
    objDecDef "myObj" char (litChar 'o'),
    constDecDef "myConst" string (litString "Imconstant"),

    printLn (int) (var "a"),
    printLn (int) (var "b"),
    printLn (int) (var "c"),
    printLn (int) (var "d")],
  
  block [
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
    printLn (float) (cot (litFloat 1.0)),
    printLn (int) (notNull (litInt 5)),
    printLn (int) (notNull (var "a")),
    printLn (int) (var "a"),
    printLn (int) (arg 5),
    printLn (int) (extVar "Lib" "var"),
    printLn (int) (self),
    printLn (int) (objVarSelf "thisOne"),
    printLn (int) (objVar (var "outer") (var "inner")),
    printLn (int) (inlineIf (litTrue) (litInt 5) (litInt 0)),
    printLn (int) (funcApp "myFunc" [(var "arg1"), (var "arg2")]),
    printLn (int) (extFuncApp "myLib" "myFunc" [(var "arg1"), (var "arg2")]),
    printLn (int) (stateObj bool [(var "arg1"), (var "arg2")]),
    printLn (int) (listStateObj bool [(var "arg1"), (var "arg2")])]]

helloElseBody :: (RenderSym repr) => repr (Body repr)
helloElseBody = bodyStatements [
  printStrLn "Hello, world",
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
  printLn (float) (log ((#~) (litFloat 2.0))),
  printLn (float) (ln (litFloat 2.0)),
  printLn (float) (exp (litFloat 2.0)),
  printLn (float) (sin (litFloat 2.0)),
  printLn (float) (cos (litFloat 2.0)),
  printLn (float) (tan (litFloat 2.0)),
  printLn (float) (tan (litFloat 2.0))]