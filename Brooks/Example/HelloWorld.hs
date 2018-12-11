module Example.HelloWorld (helloWorld) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, UnaryOp, Keyword, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), UnaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..))
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

helloWorld :: (RenderSym repr) => repr Doc
helloWorld = fileDoc (
  block [
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
    printLn (float) (log (litFloat 2.0)),
    printLn (float) (ln (litFloat 2.0)),
    printLn (float) (exp (litFloat 2.0)),
    printLn (float) (sin (litFloat 2.0)),
    printLn (float) (cos (litFloat 2.0)),
    printLn (float) (tan (litFloat 2.0))])